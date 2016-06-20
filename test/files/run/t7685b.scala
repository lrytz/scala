// TODO: should also test the output of -Xprint:erasure and -Xprint:posterasure


class A(val x: String) extends AnyVal
class B(val x: A)      extends AnyVal
class C[T](val x: T)   extends AnyVal
class D(val x: Int) extends AnyVal

object Test extends App {
  val t1 = new A("m")
  val t2 = new B(new A("mm"))
  val t3 = new B(t1)
  val t4 = t1.x
  val t5 = t2.x
  val t6 = t3.x.x
  val t7 = {
    val a = new A("m")
    a.x
  }

  val t8 = new C(1)
  val t9 = new C(new A(""))
  val t9a = t9.x
  val t9b = t9.x.x

  val t10 = new C(new C(""))
  val t10a = t10.x
  val t10b = t10.x.x
  val t11 = new C(t1)
  val t11a = t11.x
  val t11b = t11.x.x
  val t12 = new C(t2)
  val t13 = new C(t8)
  val t14 = new C(t10)

  val t15 = new C(new D(1))
  val t15a = t15.x
  val t15b = t15.x.x
}
