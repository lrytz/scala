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
      import settings.Yrewrites.domain._

      def go(choice: Choice, trans: RewriteTypingTransformer) =
        if (settings.Yrewrites.contains(choice))
          trans.transform(unit.body)

      go(breakOutOps, new BreakoutToIteratorOp(unit, state))
      go(breakOutArgs, new BreakoutArgsTraverser(unit, state))
      go(collectionSeq, new CollectionSeqTransformer(unit, state))
      go(varargsToSeq, new VarargsToSeq(unit, state))
      go(mapValues, new MapValuesRewriter(unit, state))
      go(nilaryInfix, new NilaryInfixRewriter(unit, state))
      go(unitCompanion, new UnitCompanion(unit, state))

      if (state.newImports.nonEmpty)
        new AddImports(unit, state).run(unit.body)

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

  // useful when working with range positions
  private def codeOf(pos: Position, source: SourceFile) =
    if (pos.start < pos.end) new String(source.content.slice(pos.start, pos.end))
    else {
      val line = source.offsetToLine(pos.point)
      val code = source.lines(line).next()
      val caret = " " * (pos.point - source.lineToOffset(line)) + "^"
      s"$code\n$caret"
    }

  private def checkNoOverlap(patches: Array[Patch], source: SourceFile): Boolean = {
    var ok = true
    for (Array(p1, p2) <- patches.sliding(2) if p1.span.end > p2.span.start) {
      ok = false
      val msg = s"""
        |overlapping patches;
        |
        |add `${p1.replacement}` at
        |${codeOf(p1.span, source)}
        |
        |add `${p2.replacement}` at
        |${codeOf(p2.span, source)}""".stripMargin.trim
      runReporting.warning(NoPosition, msg, WarningCategory.Other, "")
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
    if (checkNoOverlap(patches, source)) {
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
    def unapply(t: GenericApply): Some[(Tree, List[Tree], List[List[Tree]])] = {
      val applied = treeInfo.dissectApplied(t)
      Some((applied.core, applied.targs, applied.argss))
    }
  }

  private class RewriteTypingTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    var lastTopLevelContext: analyzer.Context = analyzer.NoContext
    var topLevelImportPos: Position = unit.source.position(0)

    lazy val collectionSeqModule = rootMirror.getRequiredModule("scala.collection.Seq")
    lazy val collectionIndexedSeqModule = rootMirror.getRequiredModule("scala.collection.IndexedSeq")

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
        if (dd.symbol.isSynthetic) tree
        else {
          localTyper.reenterTypeParams(dd.tparams)
          localTyper.reenterValueParams(dd.vparamss)
          try super.transform(dd)
          finally {
            val scope = localTyper.context.scope
            dd.tparams.foreach(tree => scope.unlink(tree.symbol))
            mforeach(dd.vparamss)(tree => scope.unlink(tree.symbol))
          }
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
      // don't emit plain `Seq`
      if (sym == collectionSeqModule || sym == collectionIndexedSeqModule)
        s"${qualifiedSelectTerm(sym.enclosingPackage)}.${sym.name}"
      else {
        val parts = ("_root_." + sym.fullName).split("\\.")
        val paths = List.tabulate(parts.length)(i => parts.takeRight(i + 1).mkString("."))
        paths.find { path =>
          val ref = newUnitParser(newCompilationUnit(path)).parseRule(_.expr())
          val typed = silentTyped(ref, Mode.QUALmode)
          typed.tpe.termSymbol == sym
        }.get
      }
    }

     def codeOf(pos: Position) = Rewrites.this.codeOf(pos, unit.source)

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

    // yes, no, unknown
    def isInfix(sel: Select, parseTree: ParseTree): Option[Boolean] = {
      // look at parse tree; e.g. `Foo(arg)` in source would have AST `pack.Foo.apply(arg)`, so it's a Select after
      // typer. We should not use the positions of the typer trees to go back to the source.
      val sourceSel = parseTree.index.get(sel.pos) match {
        case Some(fun: Select) => Some(fun)
        case _ => None
      }
      sourceSel.map(sel => {
        val qualEnd = sel.qualifier.pos.end
        val c = unit.source.content(unit.source.skipWhitespace(qualEnd))
        c != '.'
      })
    }

    def selectFromInfix(tree: Tree, code: String, parseTree: ParseTree, reuseParens: Boolean): List[Patch] = tree match {
      case Application(fun: Select, _, _) => selectFromInfixApply(tree, fun, code, parseTree, reuseParens)
      case _ => List(Patch(tree.pos.focusEnd, "." + code))
    }

    /**
     * Select `.code` from an apply, wrap in parens if it's infix. Example:
     *  - app: `coll.mapValues[T](fun)`
     *  - fun: `coll.mapValues`
     *  - code: `toMap`
     *
     * `app` could be infix `coll mapValues fun` in source.
     *
     * `reuseParens`: if `app` already has parens around it, whether to insert new parens or not. example:
     *    - `foo(coll mapValues fun)`      => cannot reuse parens, need `foo((coll mapValues fun).toMap)`
     *    - `(col map f).map(g)(breakOut)` => can reuse parens, `(col map f).iterator.map(g).to(T)`
     */
    def selectFromInfixApply(app: Tree, fun: Select, code: String, parseTree: ParseTree, reuseParens: Boolean): List[Patch] = {
      val patches = mutable.ListBuffer[Patch]()
      val posWithParens = if (reuseParens) withEnclosingParens(app.pos) else app.pos
      val needParens = isInfix(fun, parseTree).contains(true) && posWithParens.end == app.pos.end
      if (needParens) {
        patches += Patch(app.pos.focusStart, "(")
        patches += Patch(app.pos.focusEnd, ")." + code)
      } else {
        patches += Patch(posWithParens.focusEnd, "." + code)
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

  private class MethodMatcher(symbols: Symbol*) {
    private val byName = symbols.groupBy(_.name)
    def apply(sym: Symbol): Boolean = byName.get(sym.name).flatMap(_.find(sameName => sym.overrideChain.contains(sameName))).nonEmpty
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
          if (inferredBreakOut) {
            val targsString = {
              val renderer = new TypeRenderer(this)
              (TypeTree(definitions.AnyTpe) +: targs.tail).map(targ => renderer.apply(targ.tpe)).mkString("[", ", ", "]")
            }
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
    // not `++:`, the method doesn't exist on Iterator
    // could use `.view`, but `++:` is deprecated in Iterable on 2.13 (not in Seq), so probably not worth it
    val breakOutMethods = Set("map", "collect", "flatMap", "++", "scanLeft", "zip", "zipAll")

    // coll.fun[targs](args)(breakOut) --> coll.iterator.fun[targs](args).to(Target)
    override def transform(tree: Tree): Tree = tree match {
      case Application(Select(coll, funName), _, argss :+ List(bo @ Application(boFun, boTargs, _)))
        if boFun.symbol == breakOutSym =>
        if (coll.tpe.typeSymbol.isNonBottomSubClass(GenIterableLikeSym) &&
          breakOutMethods.contains(funName.decode)) {
          state.patches ++= selectFromInfix(coll, "iterator", state.parseTree, reuseParens = true)
          state.patches += Patch(withEnclosingParens(bo.pos), s".to(${qualifiedSelectTerm(boTargs.last.tpe.typeSymbol.companionModule)})")
          if (funName.startsWith("zip"))
            state.patches ++= selectFromInfix(argss.head.head, "iterator", state.parseTree, reuseParens = false)
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

    val isToSeq = new MethodMatcher(rootMirror.requiredClass[scala.collection.GenTraversableOnce[_]].info.decl(TermName("toSeq")))
    def addToSeq(arg: Tree) =
      !arg.tpe.typeSymbol.isNonBottomSubClass(CollectionImmutableSeq) &&
        arg.tpe.typeSymbol.isNonBottomSubClass(CollectionSeq) &&
        !PartialFunction.cond(arg) {
          case Ident(_) => definitions.isScalaRepeatedParamType(arg.symbol.tpe)
          case Select(_, _) => isToSeq(arg.symbol)
        }

    var currentFun: Symbol = null
    def withCurrent[T](fun: Symbol)(op: => T): T = {
      val old = currentFun
      currentFun = fun
      try op finally currentFun = old
    }

    override def transform(tree: Tree): Tree = tree match {
      case Typed(expr, Ident(tpnme.WILDCARD_STAR)) if addToSeq(expr) =>
        val op = if (currentFun.isJavaDefined) "toArray" else "toSeq"
        state.patches ++= selectFromInfix(expr, op, state.parseTree, reuseParens = true)
        super.transform(tree)
      case Application(fun, _, _) =>
        withCurrent(fun.symbol)(super.transform(tree))
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
    val isRewritable = new MethodMatcher(
      rootMirror.getRequiredClass("scala.collection.GenMapLike").info.decl(TermName("mapValues")),
      rootMirror.getRequiredClass("scala.collection.GenMapLike").info.decl(TermName("filterKeys")))

    val reducesMapView = new MethodMatcher(
      rootMirror.requiredClass[scala.collection.GenTraversableOnce[_]].info.decl(TermName("toMap")),
      rootMirror.requiredClass[scala.collection.GenMapLike[_, _, _]].info.decl(nme.apply))

    // no need to add `toMap` if it's already there, or in `m.mapValues(f).apply(x)`
    // curTree is the next outer tree (tracked by TypingTransformer)
    def skipRewrite = PartialFunction.cond(curTree) {
      case sel: Select => reducesMapView(sel.symbol)
    }

    override def transform(tree: Tree): Tree = {
      tree match {
        case ap @ Application(fun: Select, _, _) if isRewritable(fun.symbol) =>
          if (!skipRewrite)
            state.patches ++= selectFromInfixApply(ap, fun, code = "toMap", state.parseTree, reuseParens = false)
        case _ =>
          super.transform(tree)
      }
      tree
    }
  }

  private class NilaryInfixRewriter(unit: CompilationUnit, state: RewriteState) extends RewriteTypingTransformer(unit) {
    override def transform(tree: Tree): Tree = tree match {
      case Application(fun: Select, _, List(Nil)) if isInfix(fun, state.parseTree).contains(true) =>
        state.patches ++= {
          // skip the whitespace, so `qual foo ()` doesn't become `qual. foo ()`
          selectFromInfix(fun.qualifier, "", state.parseTree, reuseParens = true) match {
            case ps :+ p => ps :+ p.copy(span = p.span.withEnd(unit.source.skipWhitespace(p.span.end)))
          }
        }
        state.patches += Patch(fun.pos.focusEnd.withEnd(unit.source.skipWhitespace(fun.pos.end)), "")
        super.transform(tree)
      case _ =>
        super.transform(tree)
    }
  }
  private class UnitCompanion(unit: CompilationUnit, state: RewriteState) extends RewriteTypingTransformer(unit) {
    val unitModule = rootMirror.requiredModule[Unit.type]
    override def transform(tree: Tree): Tree = tree match {
      case Application(sel: Select, targs, argss) if sel.qualifier.symbol == unitModule =>
        transformTrees(targs)
        argss.foreach(transformTrees)
        state.patches += Patch(tree.pos.focusEnd, " /*TODO-2.13-migration Unit companion*/")
        tree
      case _ if tree.symbol == unitModule =>
        state.patches += Patch(tree.pos, "()")
        tree
      case _ =>
        super.transform(tree)
    }
  }
}
