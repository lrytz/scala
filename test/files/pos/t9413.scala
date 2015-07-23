class A {
  val b = ""
  def f(b: Option[String] = Some(b)) = 0
  def g(b: String = b) = 1
}
