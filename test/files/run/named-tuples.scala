import NamedTuple._
object Test extends App {
  def show(x: NamedTuple2[("name", "age"), String, Int]) = s"${x.name}: ${x.age}"

  val w1: NamedTuple2[("name", "age"), String, Int] = NamedTuple2("gandalf", 55000)
  val w2 = NamedTuple2[("name", "age")]("dumbledore", 115)
  val w3 = (name = "harry", age = 11)

  assert(show(w1) == "gandalf: 55000")
  assert(show(w2) == "dumbledore: 115")
  assert(show(w3) == "harry: 11")
}
