package scala

object NamedTuple {
  sealed trait NamedTuple[N]

  final case class NamedTuple2[N <: (String, String), +T1, +T2](_1: T1, _2: T2) extends NamedTuple[N] {
    def toTuple: (T1, T2) = (_1, _2)

    override def toString: String = s"(${_1}, ${_2})"
  }

  object NamedTuple2 {
    trait Apply2[N <: (String, String)] { def apply[T1, T2](_1: T1, _2: T2): NamedTuple2[N, T1, T2] }
    private object Apply2 extends Apply2[Nothing] {
      def apply[T1, T2](_1: T1, _2: T2) = new NamedTuple2[Nothing, T1, T2](_1, _2)
    }
    def apply[N <: (String, String)]: Apply2[N] = Apply2.asInstanceOf[Apply2[N]]
  }
}
