package scala.tools.nsc
package transform

import org.junit.Assert.assertEquals
import org.junit.{Ignore, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import scala.reflect.internal.Reporter
import scala.reflect.io.AbstractFile
import scala.tools.testing.BytecodeTesting

@RunWith(classOf[JUnit4])
class RewritesTest extends BytecodeTesting {
  override def compilerArgs: String = "-Yrewrites:_"
  import compiler.global._

  def rewrite(code: String): String = {
    val f = Files.createTempFile("source", ".scala")
    Files.write(f, code.getBytes(UTF_8))
    val source = getSourceFile(AbstractFile.getFile(f.toFile))
    val run = compiler.newRun
    if (rootMirror.getPackageIfDefined("scala.collection.compat") == NoSymbol)
      loaders.enterPackage(
        rootMirror.getPackage("scala.collection").moduleClass,
        "compat",
        new loaders.PackageLoader("compat", classPath))
    run.compileSources(List(source))
    compiler.checkReport(_.severity != Reporter.ERROR)
    new String(Files.readAllBytes(f),UTF_8).stripLineEnd
  }

  def compat(s: String) = s"import scala.collection.compat._\n$s"

  @Test def collectionSeq1(): Unit = {
    val i = compat("""class C { def foo[M[A] <: Seq[_]](f: Seq[Seq[String]]): Seq[Seq[Any]] = Seq(Seq("")) }""")
    val e = compat("""class C { def foo[M[A] <: collection.Seq[_]](f: collection.Seq[collection.Seq[String]]): collection.Seq[collection.Seq[Any]] = collection.Seq(collection.Seq("")) }""")
    assertEquals(e, rewrite(i))
  }

  @Test def collectionSeqQaulified(): Unit = {
    val i = compat("""class C { def foo(collection: Any) = Seq(); def bar() = { val collection: Any = ""; Seq() }; Seq(0)}""")
    val e = compat("""class C { def foo(collection: Any) = scala.collection.Seq(); def bar() = { val collection: Any = ""; scala.collection.Seq() }; collection.Seq(0)}""")
    assertEquals(e, rewrite(i))
  }

  @Test def varargsToSeq(): Unit = {
    def s(p: String*) =
      s"""class C {
         |  List(List(1, 2): _*)
         |  List(${p(0)}Seq(1, 2)${p(1)}: _*)
         |  List(Array(1, 2): _*)
         |}""".stripMargin
    assertEquals(compat(s("collection.", ".toSeq")), rewrite(s("", "")))
  }

  @Test def addImports(): Unit = {
    def s(p: String*) = s"import scala.collection.{mutable => m, compat}${p(0)}\n class C"
    assertEquals(s("\nimport scala.collection.compat._\n"), rewrite(s("")))
    def t = s"import scala.collection.compat._\n class C"
    assertEquals(t, rewrite(t))
  }

  @Ignore @Test def mapValues(): Unit = {
    val i = compat("""class C { def test[A, B](m: Map[A, B]) = m.mapValues(x => x) }""")
    val e = compat("""class C { def test[A, B](m: Map[A, B]) = m.mapValues(x => x).toMap }""")
    assertEquals(e, rewrite(i))
  }

  @Ignore @Test def mapValuesAfterNewLine(): Unit = {
    val i = compat("class C { def test[A, B](m: Map[A, List[B]]) = {\n val r = m\n  .toMap\n  .mapValues(_.distinct.sortBy(_.hashCode))\n r }}")
    val e = compat("class C { def test[A, B](m: Map[A, List[B]]) = {\n val r = m\n  .toMap\n  .mapValues(_.distinct.sortBy(_.hashCode)).toMap\n r }}")
    assertEquals(e, rewrite(i))
  }

  @Ignore @Test def mapValuesInfix(): Unit = {
    val i = compat("""class C { def test[A, B](m: Map[A, B]) = m mapValues { x => x } }""")
    val e = compat("""class C { def test[A, B](m: Map[A, B]) = (m mapValues { x => x }).toMap }""")
    assertEquals(e, rewrite(i))
  }

  @Ignore @Test def mapValuesInfix2(): Unit = {
    val i = compat("""class C { def test[A, B](m: Map[A, B]) = m mapValues { x => x \n} }""")
    val e = compat("""class C { def test[A, B](m: Map[A, B]) = (m mapValues { x => x \n}).toMap }""")
    assertEquals(e, rewrite(i))
  }

  @Ignore("Need to improve position hack in RewriteComment or fix scalac parser")
  @Test def mapValuesInfixComment(): Unit = {
    val i = compat("""class C { def test[A, B](m: Map[A, B]) = m mapValues { x => x /*COMMENT*/} }""")
    val e = compat("""class C { def test[A, B](m: Map[A, B]) = (m mapValues { x => x /*COMMENT*/}).toMap }""")
    assertEquals(e, rewrite(i))
  }
}
