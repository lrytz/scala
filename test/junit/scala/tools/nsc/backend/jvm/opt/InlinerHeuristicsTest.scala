package scala.tools.nsc
package backend.jvm
package opt

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.{Ignore, Test}
import scala.collection.generic.Clearable
import scala.tools.asm.Opcodes._
import org.junit.Assert._

import scala.tools.asm.tree._
import scala.tools.nsc.reporters.StoreReporter

import CodeGenTools._
import scala.tools.partest.ASMConverters
import ASMConverters._
import AsmUtils._

import BackendReporting._

import scala.collection.convert.decorateAsScala._
import scala.tools.testing.ClearAfterClass

object InlinerHeuristicsTest extends ClearAfterClass.Clearable {
  val args = "-Yopt:l:classpath -Yopt-warnings"
  var compiler = newCompiler(extraArgs = args)

  def clear(): Unit = { compiler = null }
}

@RunWith(classOf[JUnit4])
class InlinerHeuristicsTest extends ClearAfterClass {
  ClearAfterClass.stateToClear = InlinerHeuristicsTest
  val compiler = InlinerHeuristicsTest.compiler
  def compile(code: String, allowMessage: StoreReporter#Info => Boolean = _ => false): List[ClassNode] = compileClasses(compiler)(code, allowMessage = allowMessage)

  @Test @Ignore
  def postInlineDownstreamFinal(): Unit = {
    val code =
      """class C {
        |  def f: Int = 0
        |  @inline final def g = f
        |}
        |class D extends C {
        |  @inline final override def f = 1
        |  def t1 = this.g
        |}
      """.stripMargin
    val List(c, d) = compile(code)
    assertNoInvoke(getSingleMethod(d, "t1"))
  }

  @Test @Ignore
  def postInlineDownstreamKnownType(): Unit = {
    val code =
      """class C {
        |  def f: Int = 0
        |  @inline final def g = f
        |}
        |class D extends C {
        |  @inline override def f = 1
        |  def t1 = (new D).g
        |}
      """.stripMargin
    val List(c, d) = compile(code)
    assertNoInvoke(getSingleMethod(d, "t1"))
  }

  @Test @Ignore
  def postInlineHigherOrderForwardInvoke(): Unit = {
    val code =
      """class C {
        |  def a(x: Int, f: Int => Int): Int = f(x)
        |  def b(f: Int => Int): Int = a(10, f) // cannot inline `a` here
        |}
        |final class D extends C {
        |  def t1 = b(x => x + 1) // inline b then a
        |}
      """.stripMargin

    val List(c, d) = compile(code)
    assertInvoke(getSingleMethod(d, "t1"), "D", "C$$$anonfun$1")
  }

  @Test @Ignore
  def postInlineRespectInlineAnnotations(): Unit = {
    val code =
      """class C {
        |  @inline def cst = 10
        |  @noinline final def a(x: Int, f: Int => Int): Int = f(x)
        |  @inline final def b(f: Int => Int): Int = a(cst, f) // a not inlined (a is @noinline), cst not inlined (not final)
        |}
        |final class D extends C {
        |  def t1 = b(x => x + 1) // b inlined, then cst inlined, but not a (a is @noinline)
        |}
      """.stripMargin

    val List(c, d) = compile(code, allowMessage = _ => true)
    val t1 = getSingleMethod(d, "t1")
    assertInvoke(t1, "C", "a")
    assertDoesNotInvoke(t1, "cst")
  }

  @Test @Ignore
  def noInlineHigherOrderNoInvocation(): Unit = {
    val code =
      """class C {
        |  final def a(x: Int, f: Int => Int): Int = x  // f is not invoked
        |  final def b(f: Int => Int): Int = a(10, f)   // a is not inlined
        |  def t1 = b(x => x + 1)                       // b is not inlined
        |}
      """.stripMargin

    val List(c) = compile(code)
    assertInvoke(getSingleMethod(c, "b"), "C", "a")
    assertInvoke(getSingleMethod(c, "t1"), "C", "b")
  }

  @Test @Ignore
  def inlineCapturedAndInvoked(): Unit = {
    val code =
      """class C {
        |  def a(f: Int => Unit): Unit = f(10)
        |  def b(f: Int => Int): Unit = a(x => println(f(x)))
        |}
        |final class D extends C {
        |  def t1 = b(x => x + 1)
        |}
      """.stripMargin
    val List(c, d) = compile(code)
    assertInvoke(getSingleMethod(d, "t1"), "scala/Predef$", "println")
  }

  @Test @Ignore
  def inlineToEliminateRef(): Unit = {
    val code =
      """class C {
        |  def t = {
        |    var r = 0
        |    def inc() = r += 1 // not annotated @inline - heuristics should kick in
        |    inc()
        |    r
        |  }
        |}
      """.stripMargin
    val List(c) = compile(code)
    assertNoInvoke(getSingleMethod(c, "t"))
  }

  @Test @Ignore
  def closureInlineToEliminateRef(): Unit = {
    val code =
      """class C {
        |  def t = {
        |    var r = 0
        |    val f = (x: Int) => r += x
        |    f(10)
        |    r
        |  }
        |}
      """.stripMargin
    val List(c) = compile(code)
    assertNoInvoke(getSingleMethod(c, "t"))

    // TODO: local variable has wrong type (IntRef)
    val locals = getSingleMethod(c, "t").localVars.filter(_.name != "this")
    assert(locals.forall(_.desc == "I"), locals)
  }

  @Test @Ignore
  def inlineHigherOrderAndClosureToEliminateRef(): Unit = {
    val code =
      """class C {
        |  final def h(f: Int => Unit) = f(10)
        |  def t = {
        |    var r = 0
        |    h(x => r += x)
        |    r
        |  }
        |}
      """.stripMargin
    val List(c) = compile(code)
    assertNoInvoke(getSingleMethod(c, "t"))
  }

  @Test
  def boxPrimitiveFunctionArg(): Unit = {
    val code =
      """class C {
        |  def t1(n: Int) = {
        |    // generic Function2, with body method C$$$anonfun$1$adapted(String,Object)Object
        |    // after re-writing the closure, the adapted method is inlined, then boxes are removed
        |    val f = (s: String, i: Int) => i + 1
        |    f("s", n)
        |  }
        |
        |  final def h(f: (String, Int) => Int, n: Int) = f("", n)
        |
        |  def t2(n: Int) = {
        |    h((s, i) => i + 1, n)
        |  }
        |}
      """.stripMargin
    val List(c) = compile(code)

    assertEquals(getSingleMethod(c, "t1").instructions.summary,
      List(LDC, ILOAD, "C$$$anonfun$1", IRETURN))

    assertEquals(getSingleMethod(c, "t2").instructions.summary,
      List(LDC, ILOAD, "C$$$anonfun$2", IRETURN))

  }

  @Test @Ignore
  def inlineToElimMultiReturnTuple(): Unit = {
    val code =
      """class C {
        |  @inline final def m(x: Int) = (x, -x) // not annotated @inline, heuristics should kick in
        |  def t1(x: Int) = {
        |    val t = m(x)
        |    t._1 == t._2
        |  }
        |
        |  def t2(x: Int) = m(x) match { case (a, b) => a == b }
        |
        |  final def h(f: Int => (Int, Int)) = f(10)
        |  def t3 = h(x => (x, -x)) match { case (a, b) => a == b }
        |}
      """.stripMargin
    val List(c) = compile(code)
    assertNoInvoke(getSingleMethod(c, "t1"))
    assertNoInvoke(getSingleMethod(c, "t2"))
    // t3: non-specialized function is created; after rewriting the closure call, the anonfun$adapted
    // method is inlined. after that we also need to inline the non-adapted anonufn method.
    assertNoInvoke(getSingleMethod(c, "t3"))
  }

  @Test @Ignore
  def rewriteTraitCallAfterInline(): Unit = {
    val code =
      """trait T {
        |  def h(f: Int => Int) = f(10)   // h is not final
        |  @inline def m = h(x => x + 10) // cannot inline h here
        |}
        |final class C extends T {
        |  // after inlining m, we get an invokeInterface T.h, which we can re-write to the impl method.
        |  // NOTE: it doesn't make sense to invest into rewriting trait calls now: this becomes obsolete
        |  // once we have the new trait encoding in
        |  def t = m
        |}
      """.stripMargin
    val List(c, t, tC) = compile(code, allowMessage = _ => true)
    assertInvoke(getSingleMethod(c, "t"), "T$class", "T$class$$$anonfun$1")
  }

  @Test @Ignore
  def inlineMethodReturningFunctionLiteral(): Unit = {
    val code =
      """class C {
        |  final def m = (x: Int) => x + 1
        |  final def h(f: Int => Int) = f(10)
        |  def t = h(m) // inlining m and then h enables closure elim
        |}
      """.stripMargin
    val List(c) = compile(code)
    assertNoInvoke(getSingleMethod(c, "t"))
  }

  @Test
  def inlineListMap(): Unit = {
    val code =
      """class C {
        |  def t(l: List[Int]) = l.map(_ + 1)
        |}
      """.stripMargin
    val List(c) = compile(code)
    println(textify(findAsmMethod(c, "t")))
    // TODO test something
    //  - map is inlined
    //  - function is still allocated because map calls generic TraversableLike.map
  }

  @Test @Ignore
  def TODO_RENAME(): Unit = {
    val code =
      """class C {
        |  // flatMap inlined into t
        |  // map inlined into C$$$anonfun$1
        |  // NOTE: both closures are not eliminated
        |  def t(l: List[List[Int]]) = l.flatMap(_.map(_ + 1))
        |}
      """.stripMargin
    val List(c) = compile(code)
//    println(textify(findAsmMethod(c, "C$$$anonfun$1")))
  }

  // method size

  // inlining closure body to eliminate boxes
  // idea: look at closure parameter types, if there's a Ref (others), inline
  // for tuples look at closure return type

  @Test
  def orderingTODO_RENAME(): Unit = {
    trait Tp {
      def a: Int
      def b: String
    }
    object Tp {
      implicit val ord = Ordering.by((tp: Tp) => (tp.a, tp.b))
    }

    // callsite to SeqLike.sorted[B >: A](implicit ord: Ordering[B]): Repr
    // call expands to l.sorted[T](Tp.ord: Ordering[Tp])
    //
    // context in dmitry's paper
    //   - B is Tp
    //   - ord value has type Tp.ord Ordering[Tp] -- we don't get singleton types at this stage..
    //
    // context for jvm backend
    //   - function literal as argument (not the case here)
    //   - parameter types -- actual parameter may have more precise type than parameter.
    //     type may be "precise", i.e., exactly D (not a subclass) -> may enable inlining (call method on that param)
    def t(l: List[Tp]) = l.sorted

    // note: this calls a different implementation, does not use Ordering.Int
    def u(a: Array[Int]) = java.util.Arrays.sort(a)

    // bad: creates a new Array[AnyRef] with boxed values, invokes generic Arrays.sort with
    // Ordering.Int as instance of Comparable[Object] which unboxes for comparison.
    def v(a: Array[Int]) = a.sorted
  }
}
