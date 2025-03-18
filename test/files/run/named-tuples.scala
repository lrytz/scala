import NamedTuple._
object Test extends App {
  def show(x: NamedTuple2[("name", "age"), String, Int]) = s"${x.name}: ${x.age}"

  val p: NamedTuple2[("name", "age"), String, Int] = NamedTuple2("gandalf", 55000)

  assert(show(p) == "gandalf: 55000")
}
