package scala

import org.junit.Test

import scala.collection.mutable.{Iterable, ListBuffer}


class Bar(val c: Iterable[AnyRef]) extends Serializable

object SD {
  import java.io._
  import scala.util.chaining._
  def serialize(obj: AnyRef) = new ByteArrayOutputStream().tap(b => new ObjectOutputStream(b).writeObject(obj)).toByteArray
  def deserialize(a: Array[Byte]) = new ObjectInputStream(new ByteArrayInputStream(a)).readObject()
  def serializeDeserialize[T <: AnyRef](obj: T) = deserialize(serialize(obj)).asInstanceOf[T]
}

object Repro {
  class A(var b: B) extends Serializable {
    def writeReplace: Object = new AProxy(this.b)
  }

  class AProxy(val b: B) extends Serializable {
    def readResolve: AnyRef = new A(b)
  }

  class B(val a: A) extends Serializable
}

class CyclicSerializationProxyTest {
  @Test def ser(): Unit = {
    ByteBuddyDynamicPatch.install()

    val b = ListBuffer[AnyRef]()
    val bar = new Bar(b)
    b += bar
    val bc = SD.serializeDeserialize(b)
    assert(bc.head.asInstanceOf[Bar].c eq bc)
  }

  @Test def coll(): Unit = {
    ByteBuddyDynamicPatch.install()

    val b1 = ListBuffer[ListBuffer[AnyRef]]()
    val b2 = ListBuffer[AnyRef](b1)
    b1 += b2
    val b1c = SD.serializeDeserialize(b1)
    println(b1c.head.head.getClass) // DefaultSerializationProxy
    println(b1c.head.head.asInstanceOf[ListBuffer[ListBuffer[AnyRef]]].size) // CCE
    assert(b1c.head.head eq b1c)
  }

  @Test def jcoll(): Unit = {
    ByteBuddyDynamicPatch.install()

    import java.util.{ArrayList => JAL, List => JL}
    val c1 = new JAL[JL[_]]()
    val c2 = JL.of(c1)
    c1.add(c2)
    val c2c = SD.serializeDeserialize(c2)
    // CCE: ArrayList contains the CollSer instance because ArrayList.readObject deserialization
    // reads the collection values using `ObjectInputStream.readObject`, which at this moment returns the proxy
    println(c2c.get(0).get(0).size())
    assert(c2c.get(0).get(0) eq c2c)
  }

  @Test def repr(): Unit = {
    ByteBuddyDynamicPatch.install()

    import Repro._
    val a = new A(null)
    val b = new B(a)
    a.b = b
    val ac = SD.serializeDeserialize(a)
    assert(ac.b.a eq ac)
  }

  @Test def twice(): Unit = {
    val l = List(1,2,3)
    val p = (l, l)
    val p1 = SD.serializeDeserialize(p)
    assert(p1._1 eq p1._2)
  }
}
