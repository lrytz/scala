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
    // show warnings
    compiler.storeReporter.infos.foreach(info => println(info.msg))
    new String(Files.readAllBytes(f),UTF_8).stripLineEnd
  }

  @Test def collectionSeq1(): Unit = {
    val i = """class C { def foo[M[A] <: Seq[_]](f: Seq[Seq[String]]): Seq[Seq[Any]] = Seq(Seq("")) }"""
    val e = """class C { def foo[M[A] <: collection.Seq[_]](f: collection.Seq[collection.Seq[String]]): collection.Seq[collection.Seq[Any]] = collection.Seq(collection.Seq("")) }"""
    assertEquals(e, rewrite(i))
  }

  @Test def collectionSeqQaulified(): Unit = {
    val i = """class C { def foo(collection: Any) = Seq(); def bar() = { val collection: Any = ""; Seq() }; Seq(0)}"""
    val e = """class C { def foo(collection: Any) = scala.collection.Seq(); def bar() = { val collection: Any = ""; scala.collection.Seq() }; collection.Seq(0)}"""
    assertEquals(e, rewrite(i))
  }

  @Test def varargsToSeq(): Unit = {
    def s(p: String*) =
      s"""class C {
         |  List(List(1, 2): _*)
         |  List(${p(0)}Seq(1, 2)${p(1)}: _*)
         |  List(Array(1, 2): _*)
         |}""".stripMargin
    assertEquals(s("collection.", ".toSeq"), rewrite(s("", "")))
  }

  @Test def addImports(): Unit = {
    val i = "import scala.collection.{mutable => m, compat}\n class C { val s: Set[Int] = List(1).map(x => x)(collection.breakOut) }"
    val e = "import scala.collection.{mutable => m, compat}\nimport scala.collection.compat._\n class C { val s: Set[Int] = List(1).iterator.map(x => x).to(Set) }"
    assertEquals(e, rewrite(i))
  }

  @Test def addImportsExisting(): Unit = {
    val i = "import scala.collection.compat._\nclass C { val s: Set[Int] = List(1).map(x => x)(collection.breakOut) }"
    val e = "import scala.collection.compat._\nclass C { val s: Set[Int] = List(1).iterator.map(x => x).to(Set) }"
    assertEquals(e, rewrite(i))
  }

  @Test def breakOutOps1(): Unit = {
    val i = "class C { def f: Set[Int] = List(1,2,3).map(_.abs)(collection.breakOut) }"
    val e = "import scala.collection.compat._\nclass C { def f: Set[Int] = List(1,2,3).iterator.map(_.abs).to(Set) }"
    assertEquals(e, rewrite(i))
  }

  @Test def breakOutOps2(): Unit = {
    val i = "class C { def f: collection.mutable.BitSet = List(1,2,3).map(_.abs)(collection.breakOut) }"
    val e = "import scala.collection.compat._\nclass C { def f: collection.mutable.BitSet = List(1,2,3).iterator.map(_.abs).to(collection.mutable.BitSet) }"
    assertEquals(e, rewrite(i))
  }

  @Test def breakOutOps3(): Unit = {
    val i = "class C { val s: Set[Int] = List(1).map(x => x)(collection.breakOut[List[Int], Int, Set[Int]]) }"
    val e = "import scala.collection.compat._\nclass C { val s: Set[Int] = List(1).iterator.map(x => x).to(Set) }"
    assertEquals(e, rewrite(i))
  }

  @Test def mapValues(): Unit = {
    val i = """class C { def test[A, B](m: Map[A, B]) = m.mapValues(x => x) }"""
    val e = """class C { def test[A, B](m: Map[A, B]) = m.mapValues(x => x).toMap }"""
    assertEquals(e, rewrite(i))
  }

  @Test def mapValuesAfterNewLine(): Unit = {
    val i = "class C { def test[A, B](m: Map[A, List[B]]) = {\n val r = m\n  .toMap\n  .mapValues(_.distinct.sortBy(_.hashCode))\n r }}"
    val e = "class C { def test[A, B](m: Map[A, List[B]]) = {\n val r = m\n  .toMap\n  .mapValues(_.distinct.sortBy(_.hashCode)).toMap\n r }}"
    assertEquals(e, rewrite(i))
  }

  @Test def mapValuesInfix(): Unit = {
    val i = """class C { def test[A, B](m: Map[A, B]) = m mapValues { x => x } }"""
    val e = """class C { def test[A, B](m: Map[A, B]) = (m mapValues { x => x }).toMap }"""
    assertEquals(e, rewrite(i))
  }

  @Test def mapValuesInfix2(): Unit = {
    val i = "class C { def test[A, B](m: Map[A, B]) = m mapValues { x => x \n} }"
    val e = "class C { def test[A, B](m: Map[A, B]) = (m mapValues { x => x \n}).toMap }"
    assertEquals(e, rewrite(i))
  }

  @Test def mapValuesInfix3(): Unit = {
    val i = "class C { def test[A, B](m: Map[A, B], f: B => B) = m mapValues f }"
    val e = "class C { def test[A, B](m: Map[A, B], f: B => B) = (m mapValues f).toMap }"
    assertEquals(e, rewrite(i))
  }

  @Test def mapValuesInfixComment(): Unit = {
    val i = """class C { def test[A, B](m: Map[A, B]) = m mapValues { x => x /*COMMENT*/} }"""
    val e = """class C { def test[A, B](m: Map[A, B]) = (m mapValues { x => x /*COMMENT*/}).toMap }"""
    assertEquals(e, rewrite(i))
  }

  @Test def toSeqSynthetic(): Unit = {
    val i = "case class C(args: Int*)"
    assertEquals(i, rewrite(i))
  }

  @Test def varargsSeqAlready(): Unit = {
    val i =
      """class C {
        |  def a(xs: Iterator[String]) = List(xs.toSeq: _*)
        |  def b(xs: String*) = List(xs: _*)
        |  def c(xs: String*) = List(xs.map(x => x): _*)
        |  def d = List(Seq(1,2,3): _*)
        |}""".stripMargin
    // c: `toSeq` is only omitted if a varargs param is forwarded untransformed
    // d: clarifies semantics. manually rewrite to `immutable.Seq(1,2,3)` if desired.
    val e =
      """class C {
        |  def a(xs: Iterator[String]) = List(xs.toSeq: _*)
        |  def b(xs: String*) = List(xs: _*)
        |  def c(xs: String*) = List(xs.map(x => x).toSeq: _*)
        |  def d = List(collection.Seq(1,2,3).toSeq: _*)
        |}""".stripMargin
    assertEquals(e, rewrite(i))
  }

  @Test def mapValuesToMapAlready(): Unit = {
    val i = "class C { def f(m: Map[Int, Int]) = m.mapValues(_.toString).toMap }"
    assertEquals(i, rewrite(i))
  }

  @Test def toSeqInfix(): Unit = {
    val i = "class C { def f(xs: collection.Seq[Int]) = List(xs map (x => x): _*) }"
    val e = "class C { def f(xs: collection.Seq[Int]) = List((xs map (x => x)).toSeq: _*) }"
    assertEquals(e, rewrite(i))
  }

  @Test def mapValuesApply(): Unit = {
    // rewrite: `.toMap(x)` --> not what we want... we want `.toMap.apply(x)
    val i = "class C { def f(m: Map[Int, Int], x: Int) = m.mapValues(_.toString)(x) }"
    println(rewrite(i))
  }
}
