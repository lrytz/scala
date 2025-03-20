package scala

import scala.language.implicitConversions

// laundry list
//  - `(name = "bob", age = 21).toTuple match { case (name, age) => ... }` needs `toTuple`, else bindings have type `Any`
//    - do we need named patterns? probably ok without.
//  - are named tuples useful without the tuple operations that we cannot express at the type level in Scala 2?
//    - I think yes, there are good use cases, and the situation is similar with unnamed tuples
//    - ... but are named tuples without those operations enough for a query API design?
//  - Scala 3 interop, migration, cross-compilation - does it make things better or worse?

object NamedTuple {
  type NamedTuple[N]

  type NamedTuple2[N <: (String, String), +T1, +T2] = NamedTuple2.NT2[N, T1, T2]

  object NamedTuple2 {
    type NT2[N <: (String, String), +T1, +T2] <: NamedTuple[N]

    trait Apply2[N <: (String, String)] { def apply[T1, T2](_1: T1, _2: T2): NamedTuple2[N, T1, T2] }
    private object Apply2 extends Apply2[Nothing] {
      def apply[T1, T2](_1: T1, _2: T2) = (_1, _2).asInstanceOf[NamedTuple2[Nothing, T1, T2]]
    }
    def apply[N <: (String, String)]: Apply2[N] = Apply2.asInstanceOf[Apply2[N]]

    implicit final class Ops[N <: (String, String), T1, T2](private val t: NT2[N, T1, T2]) extends AnyVal {
      def toTuple: (T1, T2) = t.asInstanceOf[(T1, T2)]
    }

    // Scala 3 also allows an unnamed tuple when a named is expected
    implicit def fromTuple[N <: (String, String), T1, T2](t: (T1, T2)): NamedTuple2[N, T1, T2] =
      NamedTuple2[N][T1, T2](t._1, t._2)

    // Scala 3 also implicitly converts to tuple
    implicit def toTuple[N <: (String, String), T1, T2](n: NamedTuple2[N, T1, T2]): (T1, T2) =
      n.toTuple
  }
}
