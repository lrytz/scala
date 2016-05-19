final case class Foo(unFoo: Int) extends AnyVal
final case class Bar(unBar: Foo) extends AnyVal
final case class Baz(unBaz: Bar) extends AnyVal

final case class Thingy(unThingy: String) extends AnyVal
final case class Rocket(unRocket: Thingy) extends AnyVal
final case class Artist(unArtist: Rocket) extends AnyVal

object Test {
  def foo111 = new Foo(111)
  def foo222 = {
    val i222 = 222
    new Foo(i222)
  }

  def bar111 = new Bar(new Foo(111))
  def bar222 = {
    val i222 = 222
    new Bar(new Foo(i222))
  }
  def bar333 = {
    val f333 = new Foo(333)
    new Bar(f333)
  }

  def baz111 = new Baz(new Bar(new Foo(111)))
  def baz222 = {
    val i222 = 222
    new Baz(new Bar(new Foo(i222)))
  }
  def baz333 = {
    val f333 = new Foo(333)
    new Baz(new Bar(f333))
  }
  def baz444 = {
    val b444 = new Bar(new Foo(444))
    new Baz(b444)
  }


  def thingyAAA = new Thingy("AAA")
  def thingyBBB = {
    val sBBB = "BBB"
    new Thingy(sBBB)
  }

  def rocketAAA = new Rocket(new Thingy("AAA"))
  def rocketBBB = {
    val sBBB = "BBB"
    new Rocket(new Thingy(sBBB))
  }
  def rocketCCC = {
    val tCCC = new Thingy("CCC")
    new Rocket(tCCC)
  }

  def artistAAA = new Artist(new Rocket(new Thingy("AAA")))
  def artistBBB = {
    val sBBB = "BBB"
    new Artist(new Rocket(new Thingy(sBBB)))
  }
  def artistCCC = {
    val tCCC = new Thingy("CCC")
    new Artist(new Rocket(tCCC))
  }
  def artistDDD = {
    val rDDD = new Rocket(new Thingy("DDD"))
    new Artist(rDDD)
  }


  def main(args: Array[String]) = {
    println(foo111)
    println(foo222)
    println(bar111)
    println(bar222)
    println(bar333)
    println(baz111)
    println(baz222)
    println(baz333)
    println(baz444)

    println(patmat1(bar111))
    println(patmat1(foo111))
    println(patmat1(0))

    println(thingyAAA)
    println(thingyBBB)
    println(rocketAAA)
    println(rocketBBB)
    println(rocketCCC)
    println(artistAAA)
    println(artistBBB)
    println(artistCCC)
    println(artistDDD)

    println(patmat2(rocketAAA))
    println(patmat2(thingyAAA))
    println(patmat2(""))
  }

  def patmat1(x: Any) = x match {
    case Bar(Foo(x)) => x
    case Foo(x) => x
    case _ => -999
  }

  def patmat2(x: Any) = x match {
    case Rocket(Thingy(x)) => x
    case Thingy(x) => x
    case _ => "zzz"
  }
}
