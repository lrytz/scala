package scala


object NamedTuple {
  sealed trait NamedTuple[N]

  final case class NamedTuple2[N <: (String, String), +T1, +T2](_1: T1, _2: T2) extends NamedTuple[N] {
    def toTuple: (T1, T2) = (_1, _2)
  }
}
