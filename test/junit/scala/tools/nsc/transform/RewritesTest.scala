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

  def assertRewrites(expect: String, input: String) = {
    val obtain = rewrite(input)
    val msg = s"""=== Rewrite comparison failure ===
                 |  origin: ${input.toString.replace("\n", "\\n")}
                 |  obtain: ${obtain.toString.replace("\n", "\\n")}
                 |  expect: ${expect.toString.replace("\n", "\\n")}
                 |""".stripMargin
    assertEquals(msg, expect, obtain)
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
    val i = "class C { def f: collection.mutable.BitSet = (List(1,2,3) map (_.abs)){collection.breakOut} }"
    val e = "import scala.collection.compat._\nclass C { def f: collection.mutable.BitSet = (List(1,2,3).iterator map (_.abs)).to(collection.mutable.BitSet) }"
    assertEquals(e, rewrite(i))
  }

  @Test def breakOutOps3(): Unit = {
    val i = "class C { val s: Set[Int] = List(1).map(x => x)(collection.breakOut[List[Int], Int, Set[Int]]) }"
    val e = "import scala.collection.compat._\nclass C { val s: Set[Int] = List(1).iterator.map(x => x).to(Set) }"
    assertEquals(e, rewrite(i))
  }

  @Test def breakOutOps4(): Unit = {
    // for zip, add `.iterator` to argument
    // for `to(collection.IndexedSeq)`, make sure `IndexedSeq` has the explicit qualifier
    val i = "class C { def f(l: List[Int]): IndexedSeq[(Int, Int)] = l.zip{l map (_ + 1)}{(collection.breakOut)} }"
    val e = "import scala.collection.compat._\nclass C { def f(l: List[Int]): collection.IndexedSeq[(Int, Int)] = l.iterator.zip{(l map (_ + 1)).iterator}.to(collection.IndexedSeq) }"
    assertEquals(e, rewrite(i))
  }

  @Test def breakOutOps5(): Unit = {
    val i = "class C { def f(l: List[Int]): Set[Int] = ((l map (_ + 1)) ++ l)(collection.breakOut) }"
    val e = "import scala.collection.compat._\nclass C { def f(l: List[Int]): Set[Int] = ((l map (_ + 1)).iterator ++ l).to(Set) }"
    assertEquals(e, rewrite(i))
  }

  @Test def breakOutOps5a(): Unit = {
    val i = "class C { def f(l: List[Int]): Set[Int] = ({l map (_ + 1)} ++ l)(collection.breakOut) }"
    val e = "import scala.collection.compat._\nclass C { def f(l: List[Int]): Set[Int] = ({l map (_ + 1)}.iterator ++ l).to(Set) }"
    assertEquals(e, rewrite(i))
  }

  @Test def breakOutOps6(): Unit = {
    val i = "class C { def f(l: List[Int]): Set[Int] = (l map (_ + 1) map identity)(collection.breakOut) }"
    val e = "import scala.collection.compat._\nclass C { def f(l: List[Int]): Set[Int] = ((l map (_ + 1)).iterator map identity).to(Set) }"
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
    val i = """class C { def test[A, B](m: Map[A, B]) = println(m mapValues { x => x }) }"""
    val e = """class C { def test[A, B](m: Map[A, B]) = println((m mapValues { x => x }).toMap) }"""
    assertEquals(e, rewrite(i))
  }

  @Test def filterKeysInfix2(): Unit = {
    val i = "class C { def test[A, B](m: Map[A, B]) = m filterKeys { x => x == null \n} }"
    val e = "class C { def test[A, B](m: Map[A, B]) = (m filterKeys { x => x == null \n}).toMap }"
    assertEquals(e, rewrite(i))
  }

  @Test def mapValuesInfix3(): Unit = {
    val i = "class C { def test[A, B](m: Map[A, B], f: B => B) = m mapValues f }"
    val e = "class C { def test[A, B](m: Map[A, B], f: B => B) = (m mapValues f).toMap }"
    assertEquals(e, rewrite(i))
  }

  @Test def filterKeysInfixComment(): Unit = {
    val i = """class C { def test[A, B](m: Map[A, B]) = m filterKeys { _ == 0 /*COMMENT*/} }"""
    val e = """class C { def test[A, B](m: Map[A, B]) = (m filterKeys { _ == 0 /*COMMENT*/}).toMap }"""
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

  @Test def varargsJava(): Unit = {
    val i =
      """class C {
        |  def a(l: List[AnyRef]) = String.format("", l: _*)
        |  def b(s: collection.Seq[Object]) = String.format("", s: _*)
        |  def c(a: Array[String]) = String.format("", a: _*)
        |}""".stripMargin
    val e =
      """class C {
        |  def a(l: List[AnyRef]) = String.format("", l: _*)
        |  def b(s: collection.Seq[Object]) = String.format("", s.toArray: _*)
        |  def c(a: Array[String]) = String.format("", a: _*)
        |}""".stripMargin
    assertEquals(e, rewrite(i))
  }

  @Test def mapValuesToMapAlready(): Unit = {
    val i = "class C { def f(m: Map[Int, Int]) = m.mapValues(_.toString).toMap }"
    assertEquals(i, rewrite(i))
  }

  @Test def useGroupMap1(): Unit = {
    val i = "class C { def a[A, K, B](xs: Iterable[A])(key: A => K)(f: A => B): Map[K, Iterable[B]] = xs.groupBy(x => key(x)).mapValues { xs => xs.map(f) } }"
    val d = "class C { def a[A, K, B](xs: Iterable[A])(key: A => K)(f: A => B): Map[K, Iterable[B]] = xs.groupMap(x => key(x))(f) }"
    val e = s"import scala.collection.compat._\n$d"
    assertEquals(e, rewrite(i))
  }

  @Test def useGroupMap2(): Unit = {
    val i = "class C { def a[A, K, B](xs: Vector[A])(key: A => K)(f: A => B): Map[K, Vector[B]] = xs.groupBy(key).mapValues(xs => xs.map { x => f(x) }).toMap }"
    val d = "class C { def a[A, K, B](xs: Vector[A])(key: A => K)(f: A => B): Map[K, Vector[B]] = xs.groupMap(key) { x => f(x) } }"
    val e = s"import scala.collection.compat._\n$d"
    assertEquals(e, rewrite(i))
  }

  @Test def useGroupMap1_bug(): Unit = {
    val i =
      """class C { def a[A](xs: List[A]): Map[(String, String), List[Double]] =
        |  xs.map { _ => ("#", "#", 0.0) }
        |    .groupBy{case (a,b,c) => (a,b)}
        |    .mapValues{_.map {case (a,b,c) => c}}
        |    .toMap
        |}""".stripMargin
    val e =
      """import scala.collection.compat._
        |class C { def a[A](xs: List[A]): Map[(String, String), List[Double]] =
        |  xs.map { _ => ("#", "#", 0.0) }
        |    .groupMap{case (a,b,c) => (a,b)} {case (a,b,c) => c}
        |}""".stripMargin
    assertRewrites(e, i)
  }

  @Ignore @Test def useGroupMap3_keyParam(): Unit = {
    val i = "class C { def a[A, K, B](xs: Iterable[A])(key: A => K)(f: A => B): Map[K, Iterable[B]] = xs.groupBy[K](x => key(x)).mapValues(xs => xs.map(x => f(x))) }"
    val e = "class C { def a[A, K, B](xs: Iterable[A])(key: A => K)(f: A => B): Map[K, Iterable[B]] = xs.groupMap(x => key(x))(x => f(x)) }"
    val j = rewrite(i)
    println(s"origin: $i")
    println(s"obtain: $j")
    println(s"expect: $e")
    assertEquals(e, j)
  }

  @Ignore @Test def useGroupMap3_valuesParam(): Unit = {
    val i = "class C { def a[A, K, B](xs: Iterable[A])(key: A => K)(f: A => B): Map[K, Iterable[B]] = xs.groupBy(x => key(x)).mapValues[Iterable[B]](xs => xs.map(x => f(x))) }"
    val e = "class C { def a[A, K, B](xs: Iterable[A])(key: A => K)(f: A => B): Map[K, Iterable[B]] = xs.groupMap(x => key(x))(x => f(x)) }"
    val j = rewrite(i)
    println(s"origin: $i")
    println(s"obtain: $j")
    println(s"expect: $e")
    assertEquals(e, j)
  }

  @Ignore @Test def useGroupMap3_bothParams(): Unit = {
    val i = "class C { def a[A, K, B](xs: Iterable[A])(key: A => K)(f: A => B): Map[K, Iterable[B]] = xs.groupBy[K](x => key(x)).mapValues[Iterable[B]](xs => xs.map(x => f(x))) }"
    val e = "class C { def a[A, K, B](xs: Iterable[A])(key: A => K)(f: A => B): Map[K, Iterable[B]] = xs.groupMap(x => key(x))(x => f(x)) }"
    val j = rewrite(i)
    println(s"origin: $i")
    println(s"obtain: $j")
    println(s"expect: $e")
    assertEquals(e, j)
  }

  @Test def useGroupMap4_infixCurlies(): Unit = {
    val i = "class C { def a[A, K, B](xs: Iterable[A])(key: A => K)(f: A => B): Map[K, Iterable[B]] = xs groupBy (x => key(x)) mapValues { xs => xs.map{x => f(x)} } }"
    val d = "class C { def a[A, K, B](xs: Iterable[A])(key: A => K)(f: A => B): Map[K, Iterable[B]] = xs.groupMap (x => key(x)){x => f(x)} }"
    val e = s"import scala.collection.compat._\n$d"
    assertRewrites(e, i)
  }

  @Test def useGroupMap4_reallyInfixCurlies(): Unit = {
    val i = "class C { def a = List(1, 2) map (x => (x, x)) groupBy { case (a, _) => a } mapValues { _.map { case (_, b) => b } } }"
    val d = "class C { def a = (List(1, 2) map (x => (x, x))).groupMap { case (a, _) => a } { case (_, b) => b } }"
    val e = s"import scala.collection.compat._\n$d"
    assertRewrites(e, i)
  }

  @Test def toSeqInfix(): Unit = {
    val i = "class C { def f(xs: collection.Seq[Int]) = List(xs map (x => x): _*) }"
    val e = "class C { def f(xs: collection.Seq[Int]) = List((xs map (x => x)).toSeq: _*) }"
    assertEquals(e, rewrite(i))
  }

  @Test def toSeqInfix2(): Unit = {
    val i = "class C { def f(xs: collection.Seq[Int]) = List({xs map (x => x)}: _*) }"
    val e = "class C { def f(xs: collection.Seq[Int]) = List({xs map (x => x)}.toSeq: _*) }"
    assertEquals(e, rewrite(i))
  }

  @Test def mapValuesApply(): Unit = {
    // don't add `toMap` in `m.mapValues(f).apply(x)`, the map is discarded anyway.
    // inserting `toMap` is incorrect if the `apply` was implicit: `m.mapValues(f).toMap(x)`
    val i = "class C { def f(m: Map[Int, Int], x: Int) = m.mapValues(_.toString)(x) }"
    assertEquals(i, rewrite(i))
  }

  @Test def nilaryInfix(): Unit = {
    val i =
      """class C {
        |  def a(l: List[Int]) = l map (_+1) toString ()
        |  def b(l: List[Int]) = {l map (_+1)} toString ()
        |  def c(l: List[Int]) = l.map(_+1) toString()
        |  def d(l: List[Int]) = (l map (_+1)).toString()
        |  def e = new C().toString
        |  def f = (new C).toString
        |}""".stripMargin
    val e =
      """class C {
        |  def a(l: List[Int]) = (l map (_+1)).toString()
        |  def b(l: List[Int]) = {l map (_+1)}.toString()
        |  def c(l: List[Int]) = l.map(_+1).toString()
        |  def d(l: List[Int]) = (l map (_+1)).toString()
        |  def e = new C().toString
        |  def f = (new C).toString
        |}""".stripMargin
    assertEquals(e, rewrite(i))
  }

  @Test def unitCompanion(): Unit = {
    val i =
      """class C {
        |  def a: Unit = Unit
        |  def b = Unit.box(scala.Unit)
        |  def c = Unit.unbox(b)
        |}""".stripMargin
    val e =
      """class C {
        |  def a: Unit = ()
        |  def b = Unit.box(()) /*TODO-2.13-migration Unit companion*/
        |  def c = Unit.unbox(b) /*TODO-2.13-migration Unit companion*/
        |}""".stripMargin
    assertEquals(e, rewrite(i))
  }

  @Test def formatted(): Unit = {
    val i =
      """class C {
        |  val p = "%"
        |  def a = hashCode.toDouble.formatted(p + ".3f")
        |  def b = this.a.length formatted "%d"
        |  def c = (toString charAt 2) formatted (p + "c")
        |  def d = 1.formatted("%d")
        |  def e = a.formatted("%s")
        |  def f = hashCode.formatted("%d")
        |  def g = List(1).map(_.formatted("%d"))
        |  def h = List(1).map(_.toDouble.formatted("%.3f"))
        |  def i = (new C).hashCode formatted "%d"
        |  def j = ((this.a + "hi").hashCode).formatted("%d")
        |}""".stripMargin
    val e =
      s"""class C {
        |  val p = "%"
        |  def a = (p + ".3f").format(hashCode.toDouble)
        |  def b = f"$${this.a.length}%d"
        |  def c = (p + "c").format(toString charAt 2)
        |  def d = f"$${1}%d"
        |  def e = a
        |  def f = f"$$hashCode%d"
        |  def g = List(1).map(fmtValue => f"$$fmtValue%d")
        |  def h = List(1).map(fmtValue => f"$${fmtValue.toDouble}%.3f")
        |  def i = f"$${(new C).hashCode}%d"
        |  def j = f"$${(this.a + "hi").hashCode}%d"
        |}""".stripMargin
    assertEquals(e, rewrite(i))
  }
}
