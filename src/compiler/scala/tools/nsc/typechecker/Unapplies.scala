/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

import symtab.Flags._

/*
 *  @author  Martin Odersky
 *  @version 1.0
 */
trait Unapplies extends ast.TreeDSL
{
  self: Analyzer =>

  import global._
  import definitions._
  import CODE.{ CASE => _, _ }

  private def isVarargs(vd: ValDef) = treeInfo isRepeatedParamType vd.tpt
  private def isByName(vd: ValDef)  = treeInfo isByNameParamType vd.tpt
  private def toIdent(x: DefTree)   = Ident(x.name) setPos x.pos.focus

  /** returns type list for return type of the extraction */
  def unapplyTypeList(ufn: Symbol, ufntpe: Type) = {
    assert(ufn.isMethod)
    //Console.println("utl "+ufntpe+" "+ufntpe.typeSymbol)
    ufn.name match {
      case nme.unapply    => unapplyTypeListFromReturnType(ufntpe)
      case nme.unapplySeq => unapplyTypeListFromReturnTypeSeq(ufntpe)
      case _              => throw new TypeError(ufn+" is not an unapply or unapplySeq")
    }
  }
  /** (the inverse of unapplyReturnTypeSeq)
   *  for type Boolean, returns Nil
   *  for type Option[T] or Some[T]:
   *   - returns T0...Tn if n>0 and T <: Product[T0...Tn]]
   *   - returns T otherwise
   */
  def unapplyTypeListFromReturnType(tp1: Type): List[Type] = {
    val tp = unapplyUnwrap(tp1)
    tp.typeSymbol match {                             // unapplySeqResultToMethodSig
      case BooleanClass             => Nil
      case OptionClass | SomeClass  =>
        val prod = tp.typeArgs.head
        getProductArgs(prod) match {
          case Some(xs) if xs.size > 1  => xs         // n > 1
          case _                        => List(prod) // special n == 0 ||  n == 1
        }
      case _                        =>
        throw new TypeError("result type "+tp+" of unapply not in {Boolean, Option[_], Some[_]}")
    }
  }

  /** let type be the result type of the (possibly polymorphic) unapply method
   *  for type Option[T] or Some[T]
   *  -returns T0...Tn-1,Tn* if n>0 and T <: Product[T0...Tn-1,Seq[Tn]]],
   *  -returns R* if T = Seq[R]
   */
  def unapplyTypeListFromReturnTypeSeq(tp1: Type): List[Type] = {
    val tp = unapplyUnwrap(tp1)
    tp.typeSymbol match {
      case OptionClass | SomeClass  =>
        val ts = unapplyTypeListFromReturnType(tp1)
        val last1 = (ts.last baseType SeqClass) match {
          case TypeRef(pre, seqClass, args) => typeRef(pre, RepeatedParamClass, args) // XXX seqClass or SeqClass?
          case _                            => throw new TypeError("last not seq")
        }
        ts.init ::: List(last1)
      case _                        =>
        throw new TypeError("result type "+tp+" of unapply not in {Option[_], Some[_]}")
    }
  }

  /** returns type of the unapply method returning T_0...T_n
   *  for n == 0, boolean
   *  for n == 1, Some[T0]
   *  else Some[Product[Ti]]
   */
  def unapplyReturnTypeExpected(argsLength: Int) = argsLength match {
    case 0 => BooleanClass.tpe
    case 1 => optionType(WildcardType)
    case n => optionType(productType((List fill n)(WildcardType)))
  }

  /** returns unapply or unapplySeq if available */
  def unapplyMember(tp: Type): Symbol = (tp member nme.unapply) match {
    case NoSymbol => tp member nme.unapplySeq
    case unapp    => unapp
  }
  /** returns unapply member's parameter type. */
  def unapplyParameterType(extractor: Symbol) = {
    val ps = extractor.tpe.params
    if (ps.length == 1) ps.head.tpe.typeSymbol
    else NoSymbol
  }

  def copyUntyped[T <: Tree](tree: T): T =
    returning[T](tree.duplicate)(UnTyper traverse _)

  def copyUntypedInvariant(td: TypeDef): TypeDef = {
    val copy = treeCopy.TypeDef(td, td.mods &~ (COVARIANT | CONTRAVARIANT), td.name, td.tparams, td.rhs)

    returning[TypeDef](copy.duplicate)(UnTyper traverse _)
  }

  private def classType(cdef: ClassDef, tparams: List[TypeDef]): Tree = {
    val tycon = REF(cdef.symbol)
    if (tparams.isEmpty) tycon else AppliedTypeTree(tycon, tparams map toIdent)
  }

  private def constrParamss(cdef: ClassDef): List[List[ValDef]] = {
    val DefDef(_, _, _, vparamss, _, _) = treeInfo firstConstructor cdef.impl.body
    vparamss map (_ map copyUntyped[ValDef])
  }

  /** The return value of an unapply method of a case class C[Ts]
   *  @param param  The name of the parameter of the unapply method, assumed to be of type C[Ts]
   *  @param caseclazz  The case class C[Ts]
   */
  private def caseClassUnapplyReturnValue(param: Name, caseclazz: Symbol) = {
    def caseFieldAccessorValue(selector: Symbol): Tree = Ident(param) DOT selector

    caseclazz.caseFieldAccessors match {
      case Nil      => TRUE
      case xs       => SOME(xs map caseFieldAccessorValue: _*)
    }
  }

  /** The module corresponding to a case class; overrides toString to show the module's name
   */
  def caseModuleDef(cdef: ClassDef): ModuleDef = {
    // > MaxFunctionArity is caught in Namers, but for nice error reporting instead of
    // an abrupt crash we trim the list here.
    def primaries      = constrParamss(cdef).head take MaxFunctionArity map (_.tpt)
    def inheritFromFun = !cdef.mods.isAbstract && cdef.tparams.isEmpty && constrParamss(cdef).length == 1
    def createFun      = gen.scalaFunctionConstr(primaries, toIdent(cdef), abstractFun = true)
    def parents        = if (inheritFromFun) List(createFun) else Nil
    def toString       = DefDef(
      Modifiers(OVERRIDE | FINAL),
      nme.toString_,
      Nil,
      List(Nil),
      TypeTree(),
      Literal(Constant(cdef.name.decode)))

    companionModuleDef(cdef, parents, List(toString))
  }

  def companionModuleDef(cdef: ClassDef, parents: List[Tree] = Nil, body: List[Tree] = Nil): ModuleDef = atPos(cdef.pos.focus) {
    val allParents = parents ::: List( gen.scalaScalaObjectConstr)
    ModuleDef(
      Modifiers(cdef.mods.flags & AccessFlags | SYNTHETIC, cdef.mods.privateWithin),
      cdef.name.toTermName,
      Template(allParents, emptyValDef, NoMods, Nil, List(Nil), body, cdef.impl.pos.focus))
  }

  private val caseMods = Modifiers(SYNTHETIC | CASE)

  /** The apply method corresponding to a case class
   */
  def caseModuleApplyMeth(cdef: ClassDef): DefDef = {
    val tparams   = cdef.tparams map copyUntypedInvariant
    val cparamss  = constrParamss(cdef)
    atPos(cdef.pos.focus)(
      DefDef(caseMods, nme.apply, tparams, cparamss, classType(cdef, tparams),
        New(classType(cdef, tparams), cparamss map (_ map gen.paramToArg)))
    )
  }

  /** The unapply method corresponding to a case class
   */
  def caseModuleUnapplyMeth(cdef: ClassDef): DefDef = {
    val tparams   = cdef.tparams map copyUntypedInvariant
    val paramName = newTermName("x$0")
    val method    = constrParamss(cdef) match {
      case xs :: _ if !xs.isEmpty && isVarargs(xs.last) => nme.unapplySeq
      case _                                            => nme.unapply
    }
    val cparams   = List(ValDef(Modifiers(PARAM | SYNTHETIC), paramName, classType(cdef, tparams), EmptyTree))
    val ifNull    = if (constrParamss(cdef).head.size == 0) FALSE else REF(NoneModule)
    val body      = nullSafe({ case Ident(x) => caseClassUnapplyReturnValue(x, cdef.symbol) }, ifNull)(Ident(paramName))

    atPos(cdef.pos.focus)(
      DefDef(caseMods, method, tparams, List(cparams), TypeTree(), body)
    )
  }

  def caseClassCopyMeth(cdef: ClassDef): Option[DefDef] = {
    def isDisallowed(vd: ValDef) = isVarargs(vd) || isByName(vd)
    val cparamss  = constrParamss(cdef)
    val flat      = cparamss flatten

    if (flat.isEmpty || (cdef.symbol hasFlag ABSTRACT) || (flat exists isDisallowed)) None
    else {
      val tparams = cdef.tparams map copyUntypedInvariant
      // the parameter types have to be exactly the same as the constructor's parameter types; so it's
      // not good enough to just duplicated the (untyped) tpt tree; the parameter types are removed here
      // and re-added in ``finishWith'' in the namer.
      def paramWithDefault(vd: ValDef) =
        treeCopy.ValDef(vd, vd.mods | DEFAULTPARAM, vd.name, atPos(vd.pos.focus)(TypeTree() setOriginal vd.tpt), toIdent(vd))

      val paramss   = cparamss map (_ map paramWithDefault)
      val classTpe  = classType(cdef, tparams)

      Some(atPos(cdef.pos.focus)(
        DefDef(Modifiers(SYNTHETIC), nme.copy, tparams, paramss, classTpe,
          New(classTpe, paramss map (_ map toIdent)))
      ))
    }
  }

  /**
   * Creates a Loc location class for a case class definition.
   *
   * class CLoc[T, P <: Loc[T, _, _]](v: C, f: C => Option[P]) extends Loc[T, C, P](v, f) { self =>
   *   override def copy(v: C) = new CLoc[T, P](v, f)
   * }
   *
   */
  def addLocationClass(cdef: ClassDef): ClassDef = {
    import gen.scalaDot

    val locName = newTypeName("Loc")
    val selfName = newTermName("self")

    val selfSym = ValDef (
      Modifiers(SYNTHETIC),
      selfName,
      TypeTree(),
      EmptyTree
    )

    val tName = newTypeName("T")
    val pName = newTypeName("P")

    val _1Name = newTypeName("_$1")
    val _2Name = newTypeName("_$2")

    def unbounded = TypeBoundsTree(scalaDot(nme.Nothing.toTypeName), scalaDot(nme.Any.toTypeName))

    def existentialQual(t: Tree) =
      ExistentialTypeTree(t, List(
        TypeDef(Modifiers(DEFERRED | SYNTHETIC), _1Name, Nil, unbounded),
        TypeDef(Modifiers(DEFERRED | SYNTHETIC), _2Name, Nil, unbounded)))

    val tparams = List(
      TypeDef(
        Modifiers(PARAM),
        tName,
        Nil,
        unbounded),
      TypeDef(
        Modifiers(PARAM),
        pName,
        Nil,
        TypeBoundsTree( // P >: Nothing <: Loc[T, _, _]
          scalaDot(nme.Nothing.toTypeName),
          existentialQual(AppliedTypeTree(
            scalaDot(locName),
            List(Ident(tName), Ident(_1Name), Ident(_2Name)))))))

    val vName = newTermName("v")
    val fName = newTermName("f")

    val optionName = newTypeName("Option")

    val fType = AppliedTypeTree(
      scalaDot(nme.Function1.toTypeName),
      List(
        Ident(cdef.name.toTypeName),
        AppliedTypeTree(scalaDot(optionName), List(Ident(pName)))))

    val vparamss = List(
      ValDef(
        Modifiers(SYNTHETIC | PARAMACCESSOR | PRIVATE | LOCAL),
        vName,
        Ident(cdef.name),
        EmptyTree) ::
      ValDef(
        Modifiers(SYNTHETIC | PARAMACCESSOR | PRIVATE | LOCAL),
        fName,
        fType,
        EmptyTree) ::
      Nil
    )

    val parents = List(
      AppliedTypeTree(scalaDot(locName), List(Ident(tName), Ident(cdef.name), Ident(pName))),
      gen.scalaScalaObjectConstr)

    val superArgss = List(Ident(vName) :: Ident(fName) :: Nil)

    val classLocName = newTypeName(cdef.name + "Loc")

    // def copy(v: C) = new CLoc[P, T](v, f)
    val copyDef = DefDef(
      Modifiers(OVERRIDE),
      newTermName("copy"),
      Nil,
      List(ValDef(
        Modifiers(PARAM),
        vName,
        Ident(cdef.name),
        EmptyTree) :: Nil),
      TypeTree(),
      Apply(
        Select(
          New(AppliedTypeTree(
            Ident(classLocName),
            List(Ident(tName), Ident(pName)))),
          nme.CONSTRUCTOR),
        List(Ident(vName), Ident(fName))))

    ClassDef(
      Modifiers(cdef.mods.flags & AccessFlags | SYNTHETIC, cdef.mods.privateWithin),
      classLocName,
      tparams,
      // parents, selfSym, constrMods, vparamss, superArgss, body, superPos
      Template(parents, selfSym, NoMods, vparamss, superArgss, List(copyDef), cdef.impl.pos.focus)
    )
  }


  /**
   * For every parameter of the case class, creates a method which returns
   * the location of that parameter.
   *
   * This method is called during the type completer of the Loc class, which allows
   * looking at the symbol / type of the ClassDef.
   *
   * If the field is another case class with @zip:
   *   def fld: FldTpeLoc[T, CLoc[T, P]] = new FldTpeLoc[T, CLoc[T, P]](v.fld, (fld: FldTpe) => {
   *     Some(self.copy(v.copy(fld = fld)))
   *   })
   *
   * otherwise:
   *   def fld: Loc[T, FldTpe, CLoc[T, P]] = new Loc[T, FldTpe, CLoc[T, P]](v.fld, (fld: FldTpe) => {
   *     Some(self.copy(v.copy(fld = fld)))
   *   })

   *
   */
  def addLocationGetters(cdef: ClassDef): List[DefDef] = {

    val locName = newTypeName("Loc")
    val selfName = newTermName("self")

    val tName = newTypeName("T")
    val pName = newTypeName("P")

    val vName = newTermName("v")

    val classLocName = newTypeName(cdef.name + "Loc")

    val c = cdef.symbol.primaryConstructor
    val ps = c.paramss.flatten

    ps map (p => {
      val name = p.name
      val paramType = p.tpe
      val paramTypeTree = TypeTree(paramType)

      val args = List(
        Select(Ident(vName), name),
        Function(
          List(ValDef(
            Modifiers(PARAM),
            name,
            paramTypeTree,
            EmptyTree)),
          Apply(
            Ident(nme.Some),
            List(Apply(
              Select(Ident(selfName), newTermName("copy")),
              List(Apply(
                Select(Ident(vName), nme.copy),
                List(AssignOrNamedArg(Ident(name), Ident(name))))))))))

      if (paramType.typeSymbol.hasAnnotation(ZipAttr)) {
        val paramLocClass = paramType.typeSymbol.owner.info.member(newTypeName(paramType.typeSymbol.name + "Loc"))
        val resType = AppliedTypeTree(
          gen.mkAttributedRef(paramLocClass), // @TODO: use same prefix as paramType (see namesDefaults for how to do it)
          List(Ident(tName), AppliedTypeTree(Ident(classLocName), List(Ident(tName), Ident(pName)))))
        DefDef(
          NoMods,
          name,
          Nil,
          Nil,
          resType.duplicate,
          Apply(
            Select(
              New(resType),
              nme.CONSTRUCTOR),
            args))
      } else {
        val resType = AppliedTypeTree(
          Ident(locName),
          List(Ident(tName), paramTypeTree, AppliedTypeTree(Ident(classLocName), List(Ident(tName), Ident(pName)))))

        DefDef(
          NoMods,
          name,
          Nil,
          Nil,
          resType.duplicate,
          Apply(
            Select(
              New(resType),
              nme.CONSTRUCTOR),
            args))
      }
    })
  }

  /**
   * Adds a methods "loc" to case classes which returns
   * the location.
   *
   *   def loc = new CLoc[C, Nothing](this, v => None)
   */
  def addLocMethod(cdef: ClassDef): DefDef = {
    val classLocName = newTypeName(cdef.name + "Loc")

    DefDef(
      NoMods,
      newTermName("loc"),
      Nil,
      Nil,
      TypeTree(),
      Apply(
        Select(
          New(AppliedTypeTree(
            Ident(classLocName),
            List(Ident(cdef.name), Ident(nme.Nothing.toTypeName)))),
          nme.CONSTRUCTOR),
        List(
          This(nme.EMPTY.toTypeName),
          Function(
            List(ValDef(
              Modifiers(PARAM),
              newTermName("v"),
              TypeTree(),
              EmptyTree)),
            Ident(newTermName("None"))))))
  }
}
