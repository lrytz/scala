object Test {

  def main(args: Array[String]) = {
    val x = new X(new XX(3))
    println(x.i.x + 9)
  }

}

class X[T](val i: XX[T]) extends AnyVal
class XX[T](val x: T) extends AnyVal
