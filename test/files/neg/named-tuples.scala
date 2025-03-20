import NamedTuple._

object Test {
  val w1 = (name = "gandalf", age = 55000)
  val w2: (age: Int, name: String) = w1 // error

  val p1: (y: Int, x: Int) = (x = 1, y = 2) // error
}
