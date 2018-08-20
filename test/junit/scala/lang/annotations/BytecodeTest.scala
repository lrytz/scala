package scala.lang.annotations

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection.JavaConverters._
import scala.tools.nsc.backend.jvm.AsmUtils
import scala.tools.partest.ASMConverters._
import scala.tools.testing.BytecodeTesting
import scala.tools.testing.BytecodeTesting._

@RunWith(classOf[JUnit4])
class BytecodeTest extends BytecodeTesting {
  import compiler._

  @Test
  def bridgeFlag(): Unit = {
    val code =
      """ A { def f: Object = null }
        |object B extends A { override def f: String = "b" }
      """.stripMargin
    for (base <- List("trait", "class")) {
      val List(a, bMirror, bModule) = compileClasses(base + code)
      assertEquals("B", bMirror.name)
      assertEquals(List("f()Ljava/lang/Object;0x49", "f()Ljava/lang/String;0x9"),
        bMirror.methods.asScala
          .filter(_.name == "f")
          .map(m => m.name + m.desc + "0x" + Integer.toHexString(m.access)).toList.sorted)
    }
  }

  @Test
  def varArg(): Unit = {
    val code =
      """ A { @annotation.varargs def f(i: Int*): Object = null }
        |object B extends A { @annotation.varargs override def f(i: Int*): String = "b" }
      """.stripMargin
    for (base <- List("trait", "class")) {
      val List(a, bMirror, bModule) = compileClasses(base + code)
      assertEquals("B", bMirror.name)
      assertEquals(List(
        "f(Lscala/collection/Seq;)Ljava/lang/Object;0x49",
        "f(Lscala/collection/Seq;)Ljava/lang/String;0x9",
        "f([I)Ljava/lang/Object;0xc9",
        "f([I)Ljava/lang/String;0x89"),
        bMirror.methods.asScala
          .filter(_.name == "f")
          .map(m => m.name + m.desc + "0x" + Integer.toHexString(m.access)).toList.sorted)
    }
  }

  @Test
  def t8731(): Unit = {
    val code =
      """class C {
        |  def f(x: Int) = (x: @annotation.switch) match {
        |    case 1 => 0
        |    case 2 => 1
        |    case 3 => 2
        |  }
        |  final val K = 10
        |  def g(x: Int) = (x: @annotation.switch) match {
        |    case K => 0
        |    case 1 => 10
        |    case 2 => 20
        |  }
        |}
      """.stripMargin

    val c = compileClass(code)

    assertTrue(getInstructions(c, "f").count(_.isInstanceOf[TableSwitch]) == 1)
    assertTrue(getInstructions(c, "g").count(_.isInstanceOf[LookupSwitch]) == 1)
  }

  @Test
  def t8926(): Unit = {
    import scala.reflect.internal.util.BatchSourceFile

    // this test cannot be implemented using partest because of its mixed-mode compilation strategy:
    // partest first compiles all files with scalac, then the java files, and then again the scala
    // using the output classpath. this shadows the bug scala/bug#8926.

    val annotA =
      """import java.lang.annotation.Retention;
        |import java.lang.annotation.RetentionPolicy;
        |@Retention(RetentionPolicy.RUNTIME)
        |public @interface AnnotA { }
      """.stripMargin
    val annotB = "public @interface AnnotB { }"

    val scalaSrc =
      """@AnnotA class A
        |@AnnotB class B
      """.stripMargin

    val run = new global.Run()
    run.compileSources(List(new BatchSourceFile("AnnotA.java", annotA), new BatchSourceFile("AnnotB.java", annotB), new BatchSourceFile("Test.scala", scalaSrc)))
    val outDir = global.settings.outputDirs.getSingleOutput.get
    val outfiles = (for (f <- outDir.iterator if !f.isDirectory) yield (f.name, f.toByteArray)).toList

    def check(classfile: String, annotName: String) = {
      val f = (outfiles collect { case (`classfile`, bytes) => AsmUtils.readClass(bytes) }).head
      val descs = f.visibleAnnotations.asScala.map(_.desc).toList
      assertTrue(descs.toString, descs exists (_ contains annotName))
    }

    check("A.class", "AnnotA")

    // known issue scala/bug#8928: the visibility of AnnotB should be CLASS, but annotation classes without
    // a @Retention annotation are currently emitted as RUNTIME.
    check("B.class", "AnnotB")
  }
}