package scala.lang.valueclass

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.RunTesting

object ClassOfTest {
  class VC(val x: Any) extends AnyVal
}

@RunWith(classOf[JUnit4])
class ValueClassTest {

}
