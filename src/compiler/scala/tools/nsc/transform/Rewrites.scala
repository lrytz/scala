package scala.tools.nsc
package transform

import scala.annotation.tailrec
import scala.collection.{GenIterableLike, mutable}
import scala.reflect.internal.util.SourceFile
import scala.tools.nsc.Reporting.WarningCategory

abstract class Rewrites extends SubComponent with TypingTransformers {
  import global._

  val phaseName = "rewrites"

  def newPhase(prev: Phase): StdPhase = {
    if (settings.Yrewrites.value.isEmpty) {
      new StdPhase(prev) {
        override def apply(unit: global.CompilationUnit): Unit = ()
      }
    } else
      new RewritePhase(prev)
  }

  private class RewritePhase(prev: Phase) extends StdPhase(prev) {
    override def apply(unit: CompilationUnit): Unit = {
      val state = new RewriteState(ParseTree(unit.source))
      val settings = global.settings
      val rws = settings.Yrewrites

      if (rws.contains(rws.domain.breakOutOps)) {
        val rewriter = new BreakoutToIteratorOp(unit, state)
        rewriter.transform(unit.body)
      }
      if (rws.contains(rws.domain.breakOutArgs)) {
        val rewriter = new BreakoutArgsTraverser(unit, state)
        rewriter.transform(unit.body)
      }
      if (rws.contains(rws.domain.collectionSeq)) {
        val rewriter = new CollectionSeqTransformer(unit, state)
        rewriter.transform(unit.body)
      }
      if (rws.contains(rws.domain.varargsToSeq)) {
        val rewriter = new VarargsToSeq(unit, state)
        rewriter.transform(unit.body)
      }
      if (rws.contains(rws.domain.mapValues)) {
        val rewriter = new MapValuesRewriter(unit, state)
        rewriter.transform(unit.body)
      }
      if (state.newImports.nonEmpty) {
        val rewriter = new AddImports(unit, state)
        rewriter.run(unit.body)
      }
      writePatches(unit.source, state.patches.toArray)
    }
  }

  private class RewriteState(val parseTree: ParseTree) {
    val patches = mutable.ArrayBuffer.empty[Patch]
    val eliminatedBreakOuts = mutable.Set.empty[Tree]
    val newImports = mutable.Set.empty[NewImport]
  }

  sealed trait NewImport {
    def matches(imp: Import): Boolean
    def imp: String
  }
  object CollectionCompatImport extends NewImport {
    lazy val ScalaCollectionCompatPackage = rootMirror.getPackageIfDefined("scala.collection.compat")
    def matches(imp: global.Import): Boolean =
      imp.expr.tpe.termSymbol == ScalaCollectionCompatPackage &&
        imp.selectors.exists(_.name == nme.WILDCARD)

    val imp = "import scala.collection.compat._"
  }

  private case class Patch(span: Position, replacement: String) {
    def delta: Int = replacement.length - (span.end - span.start)
  }

  private def checkNoOverlap(patches: Array[Patch]): Boolean = {
    var ok = true
    if (patches.nonEmpty)
      patches.reduceLeft { (p1, p2) =>
        if (p1.span.end > p2.span.start) {
          ok = false
          runReporting.warning(NoPosition, s"overlapping patches:\n - $p1\n - $p2", WarningCategory.Other, "")
        }
        p2
      }
    ok
  }

  private def applyPatches(source: SourceFile, patches: Array[Patch]): String = {
    val sourceChars = source.content
    val patchedChars = new Array[Char](sourceChars.length + patches.foldLeft(0)(_ + _.delta))

    @tailrec def loop(pIdx: Int, inIdx: Int, outIdx: Int): Unit = {
      def copy(upTo: Int): Int = {
        val untouched = upTo - inIdx
        System.arraycopy(sourceChars, inIdx, patchedChars, outIdx, untouched)
        outIdx + untouched
      }
      if (pIdx < patches.length) {
        val p = patches(pIdx)
        val outNew = copy(p.span.start)
        p.replacement.copyToArray(patchedChars, outNew)
        loop(pIdx + 1, p.span.end, outNew + p.replacement.length)
      } else {
        val outNew = copy(sourceChars.length)
        assert(outNew == patchedChars.length, s"$outNew != ${patchedChars.length}")
      }
    }
    loop(0, 0, 0)
    new String(patchedChars)
  }

  private def writePatches(source: SourceFile, patches: Array[Patch]): Unit = if (patches.nonEmpty) {
    java.util.Arrays.sort(patches, Ordering.by[Patch, Int](_.span.start))
    if (checkNoOverlap(patches)) {
      val bytes = applyPatches(source, patches).getBytes(settings.encoding.value)
      val out = source.file.output
      out.write(bytes)
      out.close()
    }
  }

  class ParseTree private (val tree: Tree, val index: collection.Map[Position, Tree])
  object ParseTree {
    def apply(source: SourceFile): ParseTree = {
      val unit = new CompilationUnit(source)
      unit.body = newUnitParser(unit).parse()
      val index = mutable.HashMap[Position, Tree]()
      unit.body.foreach(x => if (!x.pos.isTransparent && x.pos.isRange) index(x.pos) = x)
      new ParseTree(unit.body, index)
    }
  }

  // Applied.unapply matches any tree, not just applications
  private object Application {
    def unapply(t: Tree): Option[(Tree, List[Tree], List[List[Tree]])] = t match {
      case _: Apply | _: TypeApply =>
        val applied = treeInfo.dissectApplied(t)
        Some((applied.core, applied.targs, applied.argss))
      case _ => None
    }
  }

  private class RewriteTypingTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    var lastTopLevelContext: analyzer.Context = analyzer.NoContext
    var topLevelImportPos: Position = unit.source.position(0)
    override def transform(tree: Tree): Tree = tree match {
      case pd: PackageDef =>
        topLevelImportPos = pd.pid.pos.focusEnd
        atOwner(tree.symbol) {
          lastTopLevelContext = localTyper.context
        }
        super.transform(tree)
      case imp: Import =>
        localTyper.context = localTyper.context.make(imp)
        val context = localTyper.context
        if (context.enclClass.owner.hasPackageFlag) {
          lastTopLevelContext = context
          topLevelImportPos = tree.pos.focusEnd
        }
        super.transform(imp)
      case tt: TypeTree if tt.original != null =>
        val saved = tt.original.tpe
        tt.original.tpe = tt.tpe
        try transform(tt.original)
        finally tt.original.setType(saved)
      case Block(stats, expr) =>
        val entered = mutable.ListBuffer[Symbol]()
        val saved = localTyper
        def enter(sym: Symbol): Unit = {
          entered += sym
          localTyper.context.scope.enter(sym)
        }
        try {
          val stats1 = stats.mapConserve { stat =>
            stat match {
              case md: MemberDef =>
                val sym = md.symbol
                if (sym != NoSymbol)
                  enter(stat.symbol)
              case imp: Import   =>
                localTyper.context = localTyper.context.make(imp)
              case _             =>
            }
            transform(stat)
          }
          val expr1  = transform(expr)
          treeCopy.Block(tree, stats1, expr1)
        } finally {
          localTyper = saved
          entered.foreach(localTyper.context.scope.unlink(_))
        }
      case dd: DefDef =>
        localTyper.reenterTypeParams(dd.tparams)
        localTyper.reenterValueParams(dd.vparamss)
        try super.transform(dd)
        finally {
          val scope = localTyper.context.scope
          dd.tparams.foreach(tree => scope.unlink(tree.symbol))
          mforeach(dd.vparamss)(tree => scope.unlink(tree.symbol))
        }
      case cd: ClassDef =>
        typer.reenterTypeParams(cd.tparams)
        // TODO: what about constructor params?
        try super.transform(cd)
        finally {
          val scope = localTyper.context.scope
          cd.tparams.foreach(tree => scope.unlink(tree.symbol))
        }
      case _ => super.transform(tree)
    }

    def silentTyped(tree: Tree, mode: Mode): Tree = {
      val typer = localTyper
      val result = typer.silent[Tree](_.typed(tree, mode))
      result match {
        case analyzer.SilentResultValue(tree: Tree) =>
          tree
        case _: analyzer.SilentTypeError =>
          EmptyTree
      }
    }

    def qualifiedSelectTerm(sym: Symbol): String = {
      val parts = ("_root_." + sym.fullName).split("\\.")
      val paths = List.tabulate(parts.length)(i => parts.takeRight(i+1).mkString("."))
      paths.find { path =>
        val ref = newUnitParser(newCompilationUnit(path)).parseRule(_.expr())
        val typed = silentTyped(ref, Mode.QUALmode)
        typed.tpe.termSymbol == sym
      }.get
    }

    // useful when working with range positions
    // def codeOf(pos: Position) = new String(unit.source.content.slice(pos.start, pos.end))

    def withEnclosingParens(pos: Position): Position = {
      @tailrec def skip(offset: Int, inc: Int): Int =
        if (unit.source.content(offset).isWhitespace) skip(offset + inc, inc) else offset

      val closingPos = skip(pos.end, 1)
      val closing = unit.source.content(closingPos)

      def checkOpening(expected: Char) = {
        val openingPos = skip(pos.start - 1, -1)
        val opening = unit.source.content(openingPos)
        if (opening == expected) withEnclosingParens(pos.withStart(openingPos).withEnd(closingPos + 1))
        else pos
      }
      if (closing == ')') checkOpening('(')
      else if (closing == '}') checkOpening('{')
      else pos
    }

    /**
     * Select `.code` from an apply, wrap in parens if it's infix. Example:
     *  - app: `coll.mapValues[T](fun)`
     *  - fun: `coll.mapValues`
     *  - code: `toMap`
     *
     * `app` could be infix `coll mapValues fun` in source.
     */
    def selectFromInfixApply(app: Apply, fun: Select, code: String): List[Patch] = {
      val patches = mutable.ListBuffer[Patch]()
      val qualEnd = fun.qualifier.pos.end
      val c = unit.source.content(unit.source.skipWhitespace(qualEnd))
      val dotted = c == '.'
      if (!dotted) {
        patches += Patch(app.pos.focusStart, "(")
        patches += Patch(app.pos.focusEnd, ")." + code)
      } else {
        patches += Patch(app.pos.focusEnd, "." + code)
      }
      patches.toList
    }
  }

  private class TypeRenderer(rewriteTransformer: RewriteTypingTransformer) extends TypeMap {
    override def apply(tp: Type): Type = tp match {
      case SingleType(pre, sym) if tp.prefix.typeSymbol.isOmittablePrefix =>
        adjust(tp, pre, sym, Mode.QUALmode)((pre1, sym1) => SingleType(pre1, sym1))
      case TypeRef(pre, sym, args) =>
        val args1 = args.mapConserve(this)
        adjust(tp, pre, sym, Mode.TAPPmode | Mode.FUNmode)((pre1, sym1) => TypeRef(pre1, sym1, args1))
      case _ =>
        mapOver(tp)
    }

    def adjust(tp: Type, pre: Type, sym: Symbol, mode: Mode)(f: (Type, Symbol) => Type): Type = {
      if (pre.typeSymbol.isOmittablePrefix || global.shorthands.contains(sym.fullName)) {
        val typedTree = rewriteTransformer.silentTyped(Ident(sym.name), mode)
        if (typedTree.symbol == sym || sym.tpeHK =:= typedTree.tpe)
          f(NoPrefix, sym)
        else {
          val dummyOwner = NoSymbol.newClassSymbol(TypeName(pre.typeSymbol.fullName))
          dummyOwner.setInfo(ThisType(dummyOwner))
          val pre1 = pre match {
            case ThisType(_) | SingleType(_, _) => SingleType(NoPrefix, dummyOwner)
            case _ => TypeRef(NoPrefix, dummyOwner, Nil)
          }
          f(pre1, sym.cloneSymbol(NoSymbol))
        }
      } else {
        mapOver(tp)
      }
    }
  }

  // Rewrites

  private object BreakoutInfo {
    lazy val breakOutSym =
      definitions.getMemberMethod(rootMirror.getPackageObject("scala.collection"), TermName("breakOut"))

    lazy val GenIterableLikeSym = rootMirror.requiredClass[GenIterableLike[_, _]]

    def isInferredArg(tree: Tree): Boolean = tree match {
      case tt: TypeTree => tt.original eq null
      case _ =>
        val pos = tree.pos
        pos.isOffset && tree.forAll(t => {
          val tpos = t.pos
          tpos == NoPosition || tpos.isOffset && tpos.point == pos.point
        })
    }
  }

  private class BreakoutArgsTraverser(unit: CompilationUnit, state: RewriteState) extends RewriteTypingTransformer(unit) {
    import BreakoutInfo._

    override def transform(tree: Tree): Tree = tree match {
      case Application(fun, targs, argss) if fun.symbol == breakOutSym =>
        if (!state.eliminatedBreakOuts(tree)) {
          val inferredBreakOut = targs.forall(isInferredArg) && mforall(argss)(isInferredArg)
          val targsString = {
            val renderer = new TypeRenderer(this)
            (TypeTree(definitions.AnyTpe) +: targs.tail).map(targ => renderer.apply(targ.tpe)).mkString("[", ", ", "]")
          }
          if (inferredBreakOut) {
            state.patches += Patch(fun.pos.focusEnd, targsString)
          }
          super.transform(fun)
        }
        tree
      case _ =>
        super.transform(tree)
    }
  }

  private class BreakoutToIteratorOp(unit: CompilationUnit, state: RewriteState) extends RewriteTypingTransformer(unit) {
    import BreakoutInfo._
    val breakOutMethods = Set("map", "collect", "flatMap", "++", "scanLeft", "zip", "zipWithIndex", "zipAll")

    private val typeRenderer = new TypeRenderer(this)

    // coll.fun[targs](args)(breakOut) --> coll.iterator.fun[targs](args).to(Target)
    override def transform(tree: Tree): Tree = tree match {
      case Application(fun @ Select(coll, funName), targs, _ :+ List(bo @ Application(boFun, boTargs, _)))
        if boFun.symbol == breakOutSym =>
        if (coll.tpe.typeSymbol.isNonBottomSubClass(GenIterableLikeSym) &&
          breakOutMethods.contains(funName.toString)) {
          state.patches += Patch(coll.pos.focusEnd, ".iterator")
          state.patches += Patch(withEnclosingParens(bo.pos), s".to(${qualifiedSelectTerm(boTargs.last.tpe.typeSymbol.companionModule)})")
          state.eliminatedBreakOuts += bo
          state.newImports += CollectionCompatImport
        }
        tree
      case _ =>
        super.transform(tree)
    }
  }

  private class VarargsToSeq(unit: CompilationUnit, state: RewriteState) extends RewriteTypingTransformer(unit) {
    val CollectionImmutableSeq = rootMirror.requiredClass[scala.collection.immutable.Seq[_]]
    val CollectionSeq = rootMirror.requiredClass[scala.collection.Seq[_]]
    override def transform(tree: Tree): Tree = tree match {
      case Typed(expr, Ident(tpnme.WILDCARD_STAR))
        if !expr.tpe.typeSymbol.isNonBottomSubClass(CollectionImmutableSeq) && expr.tpe.typeSymbol.isNonBottomSubClass(CollectionSeq) =>
        state.patches += Patch(expr.pos.focusEnd, ".toSeq")
        super.transform(tree)
      case _ =>
        super.transform(tree)
    }
  }

  /** Rewrites Idents that refer to scala.Seq/IndexedSeq as collection.Seq (or scala.collection.Seq if qualification is needed) */
  private class CollectionSeqTransformer(unit: CompilationUnit, state: RewriteState) extends RewriteTypingTransformer(unit) {
    case class Rewrite(name: String, typeAlias: Symbol, termAlias: Symbol, cls: Symbol, module: Symbol)
    val ScalaCollectionPackage = rootMirror.getPackage("scala.collection")
    def rewrite(name: String) = Rewrite(name,
      definitions.ScalaPackage.packageObject.info.decl(TypeName(name)),
      definitions.ScalaPackage.packageObject.info.decl(TermName(name)),
      rootMirror.getRequiredClass("scala.collection." + name),
      rootMirror.getRequiredModule("scala.collection." + name))
    val rewrites = List(rewrite("Seq"), rewrite("IndexedSeq"))
    override def transform(tree: Tree): Tree = {
      tree match {
        case ref: RefTree =>
          for (rewrite <- rewrites) {
            val sym = ref.symbol
            if (sym == rewrite.cls || sym == rewrite.module || sym == rewrite.termAlias || sym == rewrite.typeAlias) {
              state.parseTree.index.get(ref.pos) match {
                case Some(Ident(name)) if name.string_==(rewrite.name) =>
                  val qual: String = qualifiedSelectTerm(ScalaCollectionPackage)
                  val patchCode = qual + "." + rewrite.name
                  state.patches += Patch(ref.pos, patchCode)
                case _ =>
              }
            }
          }
        case _ =>
      }
      super.transform(tree)
    }
  }

  /** Add `import scala.collection.compat._` at the top-level */
  private class AddImports(unit: CompilationUnit, state: RewriteState) extends RewriteTypingTransformer(unit) {
    def run(tree: Tree) = {
      // RewriteTypingTransformer.transform sets up the required state (lastTopLevelContext, topLevelImportPos)
      transform(tree)
      val topLevelImports = collectTopLevel
      val toAdd = state.newImports.filterNot(newImp => topLevelImports.exists(newImp.matches))
      if (toAdd.nonEmpty) {
        val top = topLevelImportPos.point == 0
        val imps = toAdd.map(_.imp).toList.sorted.mkString(if (top) "" else "\n", "\n", if (top) "\n" else "")
        state.patches += Patch(topLevelImportPos, imps)
      }
    }

    private def collectTopLevel: List[Import] = lastTopLevelContext match {
      case analyzer.NoContext =>
        Nil
      case ctx =>
        ctx.enclosingContextChain.iterator.map(_.tree).collect {
          case imp: Import => imp
        }.toList
    }
  }

  private class MapValuesRewriter(unit: CompilationUnit, state: RewriteState)
    extends RewriteTypingTransformer(unit) {
    val GenMapLike_mapValues =
      rootMirror.getRequiredClass("scala.collection.GenMapLike").info.decl(TermName("mapValues"))

    override def transform(tree: Tree): Tree = {
      tree match {
        case ap @ Apply(TypeApply(fun: Select, _), _)
          if fun.symbol.name == GenMapLike_mapValues.name && fun.symbol.overrideChain.contains(GenMapLike_mapValues) =>
          // reporter.warning(tree.pos, show(localTyper.context.enclMethod.tree, printPositions = true))
          state.patches ++= selectFromInfixApply(ap, fun, code = "toMap")
        case _ =>
          super.transform(tree)
      }
      tree
    }
  }
}
