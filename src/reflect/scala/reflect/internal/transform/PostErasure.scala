package scala.reflect
package internal
package transform

trait PostErasure {
  val global: SymbolTable
  import global._

  object elimErasedValueType extends TypeMap {
    def apply(tp: Type): Type = tp match {
      case ConstantType(Constant(tp: Type)) => ConstantType(Constant(apply(tp)))
      case ErasedValueType(_, underlying)   => apply(underlying)
      case _                                => mapOver(tp)
    }
  }

  def transformInfo(sym: Symbol, tp: Type) = elimErasedValueType(tp)
}
