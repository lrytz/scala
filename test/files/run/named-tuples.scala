import NamedTuple._
object Test extends App {
  def show(x: NamedTuple2[("name", "age"), String, Int]) = s"${x.name}: ${x.age}"
  def wohs(x: (name: String, age: Int)) = show(x)

  type P = (name: String, age: Int)

  val w1: NamedTuple2[("name", "age"), String, Int] = NamedTuple2[("name", "age")]("gandalf", 55000)
  val w2 = NamedTuple2[("name", "age")]("dumbledore", 115)
  val w3 = (name = "harry", age = 11)
  val w4: (name: String, age: Int) = w1
  val w5: (name: String, age: Int) = ("snape", 31) // implicit fromTuple
  val w6: P = w2.toTuple     // implicit fromTuple
  val w7: (String, Int) = w6 // implicit toTuple

  assert(show(w1) == "gandalf: 55000")
  assert(show(w2) == "dumbledore: 115")
  assert(show(w3) == "harry: 11")
  assert(show(w4) == wohs(w1))
  assert(show(w5) == "snape: 31")

  assert(w3.toString == "(harry,11)")

  def p1(x: Any) = x match {
    case (name: String, age: Int) => (name = name, age = age + 7)
  }

  val o1: (name: String, age: Int) = p1(w1)
  assert(show(o1) == "gandalf: 55007")

  def p2(w: (name: String, age: Int)): (name: String, age: Int) = w.toTuple match {
    case (name, age) => (name, age + 7)
  }

  assert(p2(w1) == o1)

  def p3(w: (name: String, age: Int)) = w match {
    case (name, age) => (name, age + 7)
  }

  locally {
    def t = (n = "bo", a = 33)
    def u: Int = t match {
      case (_, a) => a
    }

    assert(u == 33)

    type Person = (n: String, a: Int)
    val p: Person = t
    def v: Int = p match {
      case (_, a) => a
    }

    assert(v == 33)

    val (_, ag) = p
    val ag1: Int = ag
    assert(ag1 == 33)

    def id: Person = p match {
      case x => x
    }
  }
}
