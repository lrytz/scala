class A(x: String, y: Int)(implicit o: String)
class B1(implicit o: String) extends A(y = 5, x = "a")
class B2(implicit o: String) extends A("a", 5)
class B3(implicit o: String) extends A(y = 5, x = "a")(o)

class ALam(x: String => String, y: Int)(implicit o: String)
class B1Lam(implicit o: String) extends ALam(y = 5, x = _ + "a")
class B2Lam(implicit o: String) extends ALam(_ + "a", 5)
class B3Lam(implicit o: String) extends ALam(y = 5, x = _ + "a")(o)

class AM[E: Manifest](val x: Unit = (), y: Unit)
class BM[E: Manifest] extends AM[E](y = ())
