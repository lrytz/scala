package scala.tools.nsc
package transform

import symtab._
import Flags._
import scala.collection.mutable

// TODO: move lazy vals bitmap creation to lazy vals phase now that lazy vals are mixed in during fields
trait AccessorSynthesis extends Transform with ast.TreeDSL {
  import global._
  import definitions._
  import CODE._

  case class BitmapInfo(symbol: Symbol, mask: Literal) {
    def storageClass: ClassSymbol = symbol.info.typeSymbol.asClass
    def defTree = ValDef(symbol, if (storageClass == BooleanClass) FALSE else ZERO)
  }

  // TODO: better way to communicate from info transform to tree transfor?
  private[this] val _bitmapInfo  = perRunCaches.newMap[Symbol, BitmapInfo]
  private[this] val _slowPathFor = perRunCaches.newMap[Symbol, Symbol]()

  trait LazyInitSymbolSynth {
    protected val clazz: Symbol

    protected def defaultPos = clazz.pos.focus
    protected def isTrait    = clazz.isTrait
    protected def hasTransientAnnot(field: Symbol) = field.accessedOrSelf hasAnnotation TransientAttr

    def needsBitmap(sym: Symbol): Boolean = !(isTrait || sym.isDeferred) && sym.isMethod && sym.isLazy && !sym.isSpecialized


    /** Examines the symbol and returns a name indicating what brand of
      * bitmap it requires.  The possibilities are the BITMAP_* vals
      * defined in StdNames.  If it needs no bitmap, nme.NO_NAME.
      *
      * bitmaps for checkinit fields are not inherited
      */
    protected def bitmapCategory(sym: Symbol): Name = {
      // ensure that nested objects are transformed
      sym.initialize

      import nme._

      if (needsBitmap(sym) && sym.isLazy)
        if (hasTransientAnnot(sym)) BITMAP_TRANSIENT else BITMAP_NORMAL
      else NO_NAME
    }


    def bitmapFor(sym: Symbol): BitmapInfo = _bitmapInfo(sym)
    protected def hasBitmap(sym: Symbol): Boolean = _bitmapInfo isDefinedAt sym


    /** Fill the map from fields to bitmap infos.
      *
      * Instead of field symbols, the map keeps their getter symbols. This makes code generation easier later.
      */
    def computeBitmapInfos(decls: List[Symbol]): List[Symbol] = {
      def doCategory(fields: List[Symbol], category: Name) = {
        val nbFields = fields.length // we know it's > 0
        val (bitmapClass, bitmapCapacity) =
        if (nbFields == 1)       (BooleanClass, 1)
        else if (nbFields <= 8)  (ByteClass, 8)
        else if (nbFields <= 32) (IntClass, 32)
        else (LongClass, 64)

        // 0-based index of highest bit, divided by bits per bitmap
        // note that this is only ever > 0 when bitmapClass == LongClass
        val maxBitmapNumber = (nbFields - 1) / bitmapCapacity

        // transient fields get their own category
        val isTransientCategory = fields.head hasAnnotation TransientAttr

        val bitmapSyms =
          (0 to maxBitmapNumber).toArray map { bitmapNumber =>
            val bitmapSym =
              clazz.newVariable(nme.newBitmapName(category, bitmapNumber).toTermName, defaultPos) setInfo bitmapClass.tpe setFlag PrivateLocal

            bitmapSym addAnnotation VolatileAttr

            if (isTransientCategory) bitmapSym addAnnotation TransientAttr

            bitmapSym
          }

        fields.zipWithIndex foreach { case (f, idx) =>
          val bitmapIdx = idx / bitmapCapacity
          val offsetInBitmap = idx % bitmapCapacity
          val mask =
            if (bitmapClass == LongClass) Constant(1L << offsetInBitmap)
            else Constant(1 << offsetInBitmap)

          _bitmapInfo(f) = BitmapInfo(bitmapSyms(bitmapIdx), Literal(mask))
        }

        bitmapSyms
      }

      decls groupBy bitmapCategory flatMap {
        case (category, fields) if category != nme.NO_NAME && fields.nonEmpty => doCategory(fields, category)
        case _ => Nil
      } toList
    }

    def slowPathFor(lzyVal: Symbol): Symbol = _slowPathFor(lzyVal)

    def newSlowPathSymbol(lzyVal: Symbol): Symbol = {
      val pos = if (lzyVal.pos != NoPosition) lzyVal.pos else defaultPos // TODO: is the else branch ever taken?
      val sym = clazz.newMethod(nme.newLazyValSlowComputeName(lzyVal.name.toTermName), pos, PRIVATE) setInfo MethodType(Nil, lzyVal.tpe.resultType)
      _slowPathFor(lzyVal) = sym
      sym
    }

  }


  trait AccessorSynthTransformation {
    protected def typedPos(pos: Position)(tree: Tree): Tree

    def accessorInitialization(clazz: Symbol, templStats: List[Tree]): LazyAccessorSynth =
      if (settings.checkInit) new CheckInitAccessorSynth(clazz, templStats)
      else new LazyAccessorSynth(clazz, templStats)

    class AccessorSynth(protected val clazz: Symbol){
      protected val _newDefs = mutable.ListBuffer[Tree]()

      def newDefs = _newDefs.toList

      /** Add tree at given position as new definition */
      protected def addDef(tree: ValOrDefDef): Unit = _newDefs += typedPos(position(tree.symbol))(tree)

      /** The position of given symbol, or, if this is undefined,
        * the position of the current class.
        */
      private def position(sym: Symbol) = if (sym.pos == NoPosition) clazz.pos else sym.pos

      /** Add new method definition.
        *
        * @param sym The method symbol.
        * @param rhs The method body.
        */
      def addDefDef(sym: Symbol, rhs: Tree = EmptyTree) = addDef(DefDef(sym, rhs))
      def addValDef(sym: Symbol, rhs: Tree = EmptyTree) = addDef(ValDef(sym, rhs))

      /** Complete `stats` with init checks and bitmaps,
        * removing any abstract method definitions in `stats` that are
        * matched by some symbol defined by a tree previously passed to `addDef`.
        */
      def implementWithNewDefs(stats: List[Tree]): List[Tree] = {
        val newDefs = _newDefs.toList
        val newSyms = newDefs map (_.symbol)
        def isNotDuplicate(tree: Tree) = tree match {
          case DefDef(_, _, _, _, _, _) =>
            val sym = tree.symbol
            !(sym.isDeferred &&
              (newSyms exists (nsym => nsym.name == sym.name && (nsym.tpe matches sym.tpe))))
          case _ => true
        }
        if (newDefs.isEmpty) stats
        else newDefs ::: (stats filter isNotDuplicate)
      }

      def accessorBody(sym: Symbol) =
        if (sym.isSetter) setterBody(sym, sym.getterIn(clazz)) else getterBody(sym)

      protected def getterBody(getter: Symbol): Tree = {
        assert(getter.isGetter)
        assert(getter.hasFlag(PARAMACCESSOR))

        fieldAccess(getter)
      }

      protected def setterBody(setter: Symbol, getter: Symbol): Tree = {
        assert(getter.hasFlag(PARAMACCESSOR), s"missing implementation for non-paramaccessor $setter in $clazz")

        Assign(fieldAccess(setter), Ident(setter.firstParam))
      }

      private def fieldAccess(accessor: Symbol) =
        Select(This(clazz), accessor.accessed)

    }


    // note: we deal in getters here, not field symbols
    protected class LazyAccessorSynth(clazz: Symbol, templStats: List[Tree]) extends AccessorSynth(clazz) with LazyInitSymbolSynth {
      protected def isUnitGetter(sym: Symbol) = sym.tpe.resultType.typeSymbol == UnitClass
      protected def thisRef    = gen.mkAttributedThis(clazz)

      // overridden in CheckInitAccessorInitialization
      def rhsWithOtherInitCheck(sym: Symbol)(rhs: Tree): Tree = rhs

      /** Complete lazy field accessors. Applies only to classes,
        * for its own (non inherited) lazy fields.
        */
      def rhsWithInitCheck(sym: Symbol, slowPathDef: DefDef)(rhs: Tree): Tree =
        if (sym.isLazy && needsBitmap(sym)) {
          // only act on lazy symbols -- the others are dealt with in rhsWithOtherInitCheck
          // TODO: sharing the rhs tree `slowPathDef.rhs` with the slow path def seems to be needed to avoid duplicating closures in lazyvals,
          // when you do duplicate the rhs, you end up with duplicated lambda body methods for the lambda
          typedPos(rhs.pos)(If(mkTest(sym), Apply(Select(thisRef, slowPathDef.symbol), Nil), slowPathDef.rhs))
        }
        else rhsWithOtherInitCheck(sym)(rhs)

      /** Return an (untyped) tree of the form 'clazz.this.bitmapSym & mask (==|!=) 0', the
        * precise comparison operator depending on the value of 'equalToZero'.
        */
      protected def mkTest(field: Symbol, equalToZero: Boolean = true): Tree = {
        val bitmap = bitmapFor(field)
        val bitmapTree  = thisRef DOT bitmap.symbol

        if (bitmap.storageClass == BooleanClass) {
          if (equalToZero) NOT(bitmapTree) else bitmapTree
        } else {
          val lhs = bitmapTree GEN_&(bitmap.mask, bitmap.storageClass)
          if (equalToZero) lhs GEN_==(ZERO, bitmap.storageClass)
          else lhs GEN_!=(ZERO, bitmap.storageClass)
        }
      }

      /* Return an (untyped) tree of the form 'Clazz.this.bmp = Clazz.this.bmp | mask'. */
      def mkSetFlag(valSym: Symbol): Tree = {
        val bitmap = bitmapFor(valSym)
        def x = thisRef DOT bitmap.symbol

        // NOTE: bitwise or (`|`) on two bytes yields and Int (TODO: why was this not a problem when this ran during mixins?)
        Assign(x,
          if (bitmap.storageClass == BooleanClass) TRUE
          else Apply(Select(x, getMember(bitmap.storageClass, nme.OR)), List(bitmap.mask))
        )

      }

      /** Synthesize the compute method:
        *
        * ```
        * def l$compute() = {
        *   synchronized(this) {
        *     if ((bitmap$n & MASK) == 0) {
        *      init // l$ = <rhs>
        *      bitmap$n = bimap$n | MASK
        *     }
        *   }
        *   ...
        *   this.f1 = null
        *   ...
        *   this.fn = null
        *   l$
        * }
        * ```
        *
        * `bitmap$n` is a byte, int or long value acting as a bitmap of initialized values.
        * The kind of the bitmap determines how many bit indicators for lazy vals are stored in it.
        * For Int bitmap it is 32 and then 'n' in the above code is: (offset / 32),
        * the MASK is (1 << (offset % 32)).
        *
        * If the class contains only a single lazy val then the bitmap is
        * represented as a Boolean and the condition checking is a simple bool test.
        *
        * Private fields used only in this initializer are subsequently set to null.
        */
      def mkLazySlowPathDef(lzyVal: Symbol, rhs: Tree): DefDef = {
        val (init, retVal) =
          rhs match {
            case rhs if isUnitGetter(lzyVal) => (List(rhs), UNIT)
            case Block(stats, res) => (stats, Select(thisRef, res.symbol))
            case _ => (null, null)
          }

        if (init == null) null
        else {
          def nullify(sym: Symbol) = Select(thisRef, sym.accessedOrSelf) === LIT(null)
          val nulls = lazyValNullables.getOrElse(lzyVal, Nil) map nullify

          if (nulls.nonEmpty)
            log("nulling fields inside " + lzyVal + ": " + nulls)

          val statsToSynch = init ::: List(mkSetFlag(lzyVal), UNIT)
          val synchedRhs = gen.mkSynchronizedCheck(thisRef, mkTest(lzyVal), statsToSynch, nulls)

          val slowPathSym = slowPathFor(lzyVal)

          // TODO: this code used to run after classes were flattened -- do we have all the needed owner changes?
          DefDef(slowPathSym, Block(List(synchedRhs.changeOwner(lzyVal -> slowPathSym)), retVal))
        }
      }

      /** Map lazy values to the fields they should null after initialization. */
      private lazy val lazyValNullables: Map[Symbol, List[Symbol]] = {
        // if there are no lazy fields, take the fast path and save a traversal of the whole AST
        if (!clazz.info.decls.exists(_.isLazy)) Map()
        else {
          // A map of single-use fields to the lazy value that uses them during initialization.
          // Each field has to be private and defined in the enclosing class, and there must
          // be exactly one lazy value using it.
          //
          // Such fields will be nulled after the initializer has memoized the lazy value.
          val singleUseFields: Map[Symbol, List[Symbol]] = {
            val usedIn = mutable.HashMap[Symbol, List[Symbol]]() withDefaultValue Nil

            object SingleUseTraverser extends Traverser {
              override def traverse(tree: Tree) {
                tree match {
                  case Assign(lhs, rhs) => traverse(rhs) // assignments don't count
                  case _ =>
                    if (tree.hasSymbolField && tree.symbol != NoSymbol) {
                      val sym = tree.symbol
                      if ((sym.hasAccessorFlag || (sym.isTerm && !sym.isMethod))
                        && sym.isPrivate
                        && !(currentOwner.isGetter && currentOwner.accessed == sym) // getter
                        && !definitions.isPrimitiveValueClass(sym.tpe.resultType.typeSymbol)
                        && sym.owner == clazz
                        && !sym.isLazy
                        && !tree.isDef) {
                        debuglog("added use in: " + currentOwner + " -- " + tree)
                        usedIn(sym) ::= currentOwner
                      }
                    }
                    super.traverse(tree)
                }
              }
            }
            templStats foreach SingleUseTraverser.apply
            debuglog("usedIn: " + usedIn)
            usedIn filter {
              case (_, member :: Nil) => member.isValue && member.isLazy
              case _ => false
            } toMap
          }

          val map = mutable.Map[Symbol, Set[Symbol]]() withDefaultValue Set()
          // check what fields can be nulled for
          for ((field, users) <- singleUseFields; lazyFld <- users if !lazyFld.accessed.hasAnnotation(TransientAttr))
            map(lazyFld) += field

          map.mapValues(_.toList sortBy (_.id)).toMap
        }
      }
    }


    protected class CheckInitAccessorSynth(clazz: Symbol, templStats: List[Tree]) extends LazyAccessorSynth(clazz, templStats) {
      /** Does this field require an initialized bit?
        * Note: fields of classes inheriting DelayedInit are not checked.
        * This is because they are neither initialized in the constructor
        * nor do they have a setter (not if they are vals anyway). The usual
        * logic for setting bitmaps does therefore not work for such fields.
        * That's why they are excluded.
        * Note: The `checkinit` option does not check if transient fields are initialized.
        */
      private def needsInitFlag(sym: Symbol): Boolean = (
        sym.isGetter
          && !sym.isInitializedToDefault
          && !isConstantType(sym.info.finalResultType) // SI-4742
          && !sym.hasFlag(PARAMACCESSOR | SPECIALIZED | LAZY)
          && !sym.accessed.hasFlag(PRESUPER)
          && !sym.isOuterAccessor
          && !(sym.owner isSubClass DelayedInitClass)
          && !(sym.accessed hasAnnotation TransientAttr)
        )

      private def needsInitFlagAndHasBitmap(sym: Symbol) = hasBitmap(sym) && needsInitFlag(sym)

      //      override protected def isFieldWithTransientBitmap(field: Symbol) =
      //        super.isFieldWithTransientBitmap(field) || needsInitFlag(field) && !field.isDeferred && isTransientField(field)

      /** Examines the symbol and returns a name indicating what brand of
        * bitmap it requires.  The possibilities are the BITMAP_* vals
        * defined in StdNames.  If it needs no bitmap, nme.NO_NAME.
        *
        * bitmaps for checkinit fields are not inherited
        */
      override protected def bitmapCategory(sym: Symbol): Name = {
        import nme._

        super.bitmapCategory(sym) match {
          case NO_NAME if needsInitFlag(sym) && !sym.isDeferred =>
            if (hasTransientAnnot(sym)) BITMAP_CHECKINIT_TRANSIENT else BITMAP_CHECKINIT
          case category => category
        }
      }

      object addInitBitsTransformer extends Transformer {
        private def checkedGetter(lhs: Tree)(pos: Position) = {
          val sym = clazz.info decl lhs.symbol.getterName suchThat (_.isGetter)
          if (needsInitFlagAndHasBitmap(sym)) {
            debuglog("adding checked getter for: " + sym + " " + lhs.symbol.flagString)
            List(typedPos(pos)(mkSetFlag(sym)))
          }
          else Nil
        }
        override def transformStats(stats: List[Tree], exprOwner: Symbol) = {
          // !!! Ident(self) is never referenced, is it supposed to be confirming
          // that self is anything in particular?
          super.transformStats(
            stats flatMap {
              case stat@Assign(lhs@Select(This(_), _), rhs) => stat :: checkedGetter(lhs)(stat.pos.focus)
              // remove initialization for default values -- TODO is this case ever hit? constructors does not generate Assigns with EmptyTree for the rhs AFAICT
              case Apply(lhs@Select(Ident(self), _), EmptyTree.asList) if lhs.symbol.isSetter => Nil
              case stat                                                                       => List(stat)
            },
            exprOwner
          )
        }
      }


      override def needsBitmap(sym: Symbol): Boolean = super.needsBitmap(sym) || !(isTrait || sym.isDeferred) && needsInitFlag(sym)

      /** Make getters check the initialized bit, and the class constructor & setters are changed to set the initialized bits. */
      override def rhsWithOtherInitCheck(sym: Symbol)(rhs: Tree): Tree = {
        // Add statements to the body of a constructor to set the 'init' bit for each field initialized in the constructor
        if (sym.isConstructor) addInitBitsTransformer transform rhs
        else if (isTrait || rhs == EmptyTree) rhs
        // getter
        else if (needsInitFlag(sym)) mkCheckedAccessor(if (isUnitGetter(sym)) UNIT else rhs, rhs.pos, sym)
        else if (sym.isSetter) {
          val getter = sym.getterIn(clazz)
          if (needsInitFlag(getter)) Block(List(rhs, typedPos(rhs.pos.focus)(mkSetFlag(getter))), UNIT)
          else rhs
        }
        else rhs
      }

      def mkCheckedAccessor(retVal: Tree, pos: Position, getter: Symbol): Tree = {
        val msg = s"Uninitialized field: ${clazz.sourceFile}: ${pos.line}"
        val result =
          IF(mkTest(getter, equalToZero = false)).
            THEN(retVal).
            ELSE(Throw(NewFromConstructor(UninitializedFieldConstructor, LIT(msg))))

        typedPos(pos)(BLOCK(result, retVal))
      }

      // TODO: need to run this on all accessor bodies (after fields refactoring, this is only run on accessors mixed in during mixins, which is only PRESUPER | PARAMACCESSOR)
      override def getterBody(getter: Symbol) = {
        if (!needsInitFlag(getter)) super.getterBody(getter)
        else mkCheckedAccessor(super.getterBody(getter), getter.pos, getter)
      }

      override def setterBody(setter: Symbol, getter: Symbol) = {
        if (!needsInitFlag(getter)) super.setterBody(setter, getter)
        else Block(List(super.setterBody(setter, getter)), mkSetFlag(getter))
      }
    }
  }
}
