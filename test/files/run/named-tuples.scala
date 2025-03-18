import NamedTuple._
object Test extends App {
  def show(x: NamedTuple2[("name", "age"), String, Int]) = s"${x.name}: ${x.age}"
  def wohs(x: (name: String, age: Int)) = show(x)

  type P = (name: String, age: Int)

  val w1: NamedTuple2[("name", "age"), String, Int] = NamedTuple2("gandalf", 55000)
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

  assert(w3.toString == "(harry, 11)")
}
