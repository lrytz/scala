
//scalac: -Xlint -Werror

trait Test {

  trait T[_]

  class C {
    def -:(x: AnyRef): T[x.type] = ???
    def +:(x: AnyRef)(i: Int): T[x.type] = ???
  }

  val c = new C
  val x: AnyRef = ???

  def ok: T[x.type] = c.-:(x)
  def no: T[x.type] = x -: c

  def ok2: (Int => T[x.type]) = c.+:(x) _
  def no2: (Int => T[x.type]) = (x +: c) _
  def no3: (Int => T[x.type]) = (x +: c)(_)
}

class A {
  def /: (z: A)(op: Int): A = ???
  def self = this
  def bar(x: A, y: A) = (x.self /: y.self)(1)
}

class B {
  @annotation.nowarn
  def f(xs: List[Int]) = (0 /: xs) _
  def g = f(List(1,2,3,4))
  def test = g(_ + _)
}

// issue 11117
class A2[B2](val b: B2) { def c: List[b.type] = b :: Nil }

// don't bug me about adapting a tuple when I wrote a tuple arg to infix
class `lukas warned me about this and I brushed him off but that was last year before pandemic` {
  def `tuple in infix expression must not warn` = (42, 27) :: Nil
}
