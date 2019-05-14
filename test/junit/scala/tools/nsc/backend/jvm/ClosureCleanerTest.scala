package scala.tools.nsc.backend.jvm

import java.io.NotSerializableException

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.AssertUtil.assertThrows

// A non-serializable class we create in closures to make sure that we aren't
// keeping references to unneeded variables from our outer closures.
class NonSerializable(val id: Int = -1) {
  override def hashCode(): Int = id

  override def equals(other: Any): Boolean = {
    other match {
      case o: NonSerializable => id == o.id
      case _ => false
    }
  }
}

@RunWith(classOf[JUnit4])
class ClosureCleanerTest {
  private val someSerializableValue = 1
  private val someNonSerializableValue = new NonSerializable
  private def someSerializableMethod() = 1
  private def someNonSerializableMethod() = new NonSerializable

  /** Assert that the given closure is serializable (or not). */
  private def assertSerializable(closure: AnyRef, serializable: Boolean): Unit = {
    if (serializable) {
      Utils.serializeDeserialize(closure)
    } else {
      assertThrows[NotSerializableException] {
        Utils.serializeDeserialize(closure)
      }
    }
  }

  /**
    * Helper method for testing whether closure cleaning works as expected.
    * This cleans the given closure twice, with and without transitive cleaning.
    *
    * @param closure closure to test cleaning with
    * @param serializableBefore if true, verify that the closure is serializable
    *                           before cleaning, otherwise assert that it is not
    * @param serializableAfter if true, assert that the closure is serializable
    *                          after cleaning otherwise assert that it is not
    */
  private def verifyCleaning(
      closure: AnyRef,
      serializableBefore: Boolean,
      serializableAfter: Boolean): Unit = {
    verifyCleaning(closure, serializableBefore, serializableAfter, transitive = true)
    verifyCleaning(closure, serializableBefore, serializableAfter, transitive = false)
  }

  /** Helper method for testing whether closure cleaning works as expected. */
  private def verifyCleaning(
      closure: AnyRef,
      serializableBefore: Boolean,
      serializableAfter: Boolean,
      transitive: Boolean): Unit = {
    assertSerializable(closure, serializableBefore)
    // If the resulting closure is not serializable even after
    // cleaning, we expect ClosureCleaner to throw a SparkException
    if (serializableAfter) {
      ClosureCleaner.clean(closure, checkSerializable = true, transitive)
    } else {
      assertThrows[SparkException] {
        ClosureCleaner.clean(closure, checkSerializable = true, transitive)
      }
    }
    assertSerializable(closure, serializableAfter)
  }

  @Test
  def `clean basic serializable closures`(): Unit = {
    val localValue = someSerializableValue
    val closure1 = () => 1
    val closure2 = () => Array[String]("a", "b", "c")
    val closure3 = (s: String, arr: Array[Long]) => s + arr.mkString(", ")
    val closure4 = () => localValue
    val closure5 = () => new NonSerializable(5) // we're just serializing the class information
    val closure1r = closure1()
    val closure2r = closure2()
    val closure3r = closure3("g", Array(1, 5, 8))
    val closure4r = closure4()
    val closure5r = closure5()

    verifyCleaning(closure1, serializableBefore = true, serializableAfter = true)
    verifyCleaning(closure2, serializableBefore = true, serializableAfter = true)
    verifyCleaning(closure3, serializableBefore = true, serializableAfter = true)
    verifyCleaning(closure4, serializableBefore = true, serializableAfter = true)
    verifyCleaning(closure5, serializableBefore = true, serializableAfter = true)

    // Verify that closures can still be invoked and the result still the same
    assert(closure1() == closure1r)
    assert(closure2() sameElements closure2r)
    assert(closure3("g", Array(1, 5, 8)) == closure3r)
    assert(closure4() == closure4r)
    assert(closure5() == closure5r)
  }
}
