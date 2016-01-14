/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend.jvm
package opt

import collection.mutable
import scala.tools.asm.tree.MethodNode
import scala.tools.nsc.backend.jvm.BTypes.InternalName
import scala.collection.convert.decorateAsScala._
import scala.tools.nsc.backend.jvm.BackendReporting.OptimizerWarning

class InlinerHeuristics[BT <: BTypes](val bTypes: BT) {
  import bTypes._
  import inliner._
  import callGraph._

  case class InlineRequest(callsite: Callsite, post: List[InlineRequest]) {
    // invariant: all post inline requests denote callsites in the callee of the main callsite
    for (pr <- post) assert(pr.callsite.callsiteMethod == callsite.callee.get.callee, s"Callsite method mismatch: main $callsite - post ${pr.callsite}")
  }

  /**
   * Select callsites from the call graph that should be inlined, grouped by the containing method.
   * Cyclic inlining requests are allowed, the inliner will eliminate requests to break cycles.
   */
  def selectCallsitesForInlining: Map[MethodNode, Set[InlineRequest]] = {
    // We should only create inlining requests for callsites being compiled (not for callsites in
    // classes on the classpath). The call graph may contain callsites of classes parsed from the
    // classpath. In order to get only the callsites being compiled, we start at the map of
    // compilingClasses in the byteCodeRepository.
    val compilingMethods = for {
      classNode  <- byteCodeRepository.compilingClasses.valuesIterator
      methodNode <- classNode.methods.iterator.asScala
    } yield methodNode

    compilingMethods.map(methodNode => {
      var requests = Set.empty[InlineRequest]
      callGraph.callsites(methodNode).valuesIterator foreach {
        case callsite @ Callsite(_, _, _, Right(Callee(callee, calleeDeclClass, safeToInline, _, canInlineFromSource, calleeAnnotatedInline, _, _, callsiteWarning)), _, _, _, pos, _, _) =>
          inlineRequest(callsite) match {
            case Some(Right(req)) => requests += req
            case Some(Left(w))    =>
              if ((calleeAnnotatedInline && bTypes.compilerSettings.YoptWarningEmitAtInlineFailed) || w.emitWarning(compilerSettings)) {
                val annotWarn = if (calleeAnnotatedInline) " is annotated @inline but" else ""
                val msg = s"${BackendReporting.methodSignature(calleeDeclClass.internalName, callee)}$annotWarn could not be inlined:\n$w"
                backendReporting.inlinerWarning(callsite.callsitePosition, msg)
              }

            case None =>
              if (canInlineFromSource && calleeAnnotatedInline && !callsite.annotatedNoInline && bTypes.compilerSettings.YoptWarningEmitAtInlineFailed) {
                // if the callsite is annotated @inline, we report an inline warning even if the underlying
                // reason is, for example, mixed compilation (which has a separate -Yopt-warning flag).
                def initMsg = s"${BackendReporting.methodSignature(calleeDeclClass.internalName, callee)} is annotated @inline but cannot be inlined"
                def warnMsg = callsiteWarning.map(" Possible reason:\n" + _).getOrElse("")
                if (doRewriteTraitCallsite(callsite))
                  backendReporting.inlinerWarning(pos, s"$initMsg: the trait method call could not be rewritten to the static implementation method." + warnMsg)
                else if (!safeToInline)
                  backendReporting.inlinerWarning(pos, s"$initMsg: the method is not final and may be overridden." + warnMsg)
                else
                  backendReporting.inlinerWarning(pos, s"$initMsg." + warnMsg)
              } else if (callsiteWarning.isDefined && callsiteWarning.get.emitWarning(compilerSettings)) {
                // when annotatedInline is false, and there is some warning, the callsite metadata is possibly incomplete.
                backendReporting.inlinerWarning(pos, s"there was a problem determining if method ${callee.name} can be inlined: \n"+ callsiteWarning.get)
              }
          }

        case Callsite(ins, _, _, Left(warning), _, _, _, pos, _, _) =>
          if (warning.emitWarning(compilerSettings))
            backendReporting.inlinerWarning(pos, s"failed to determine if ${ins.name} should be inlined:\n$warning")
      }
      (methodNode, requests)
    }).filterNot(_._2.isEmpty).toMap
  }



  //
  val inlineRequests = mutable.Set.empty[InlineRequest]
  val decidedNotToInline = mutable.Set.empty[Callsite]
  val cannotInlineFirstHand = mutable.Set.empty[Callsite]

  def partitionLeftRight[A, B](c: Iterable[Either[A, B]]): (List[A], List[B]) = {
    val l = mutable.ListBuffer.empty[A]
    val r = mutable.ListBuffer.empty[B]
    c foreach {
      case Left(a)  => l += a
      case Right(b) => r += b
    }
    (l.toList, r.toList)
  }

  /**
   * What happens with the callee's parameter at `functionIndex`? check its consumers.
   *   - sam is invoked
   *     => create inline request, no additional post request needed
   *        (TODO: depends, for example if a IntRef is captured by the closure, need to inline the anonfun)
   *   - value is forwarded as an argument to another method
   *     => check how the function is used in the callee. pass more callsite knowledge.
   *   - value is captured by another closure
   *     => check how it's used within the closure body
   *     => check how the new closure is used: invoked, passed along, captured (recurisve)
   *
   * If there are multiple consumers, should we still inline? Yes: making a callsite monomorphic
   * is valuable, even if some polymorphic callsites or other uses of the closure remain.
   *
   * No need to re-run ProdCons (we ran it during call graph construction):
   *   - sam invoked: find a callsite where the receiver ArgInfo is ForwardedParam(function)
   *   - value passed on: find a callsite where a parameter ArgInfo is ForwardedParam(function)
   *   - value captured: find a closure instantiation where a parameter ArgInfo is ForwardedParam(function)
   */
  def inlineMegamorphicCallsite(callsite: Callsite, functionIndex: Int): Either[List[OptimizerWarning], Option[InlineRequest]] = {
    val callee = callsite.callee.get

    var shouldInline = false

    def isSamInvocation(cs: Callsite) = {
      val funIsReceiver = cs.argInfos.get(0).contains(ForwardedParam(functionIndex))
      var samMethodNameInvoked = false
      for {
        samType <- callee.samParamTypes.get(functionIndex)
        samTypeInfo <- samType.info
        samNameAndType <- samTypeInfo.inlineInfo.sam
        csCallee <- cs.callee
      } {
        val samName = samNameAndType.takeWhile(_ != '(')
        // TODO doc: startsWith for fuzzyness - helps with specialization
        if (csCallee.callee.name.startsWith(samName)) samMethodNameInvoked = true
      }
      funIsReceiver && samMethodNameInvoked
    }

    val downstream: Iterable[Either[OptimizerWarning, InlineRequest]] = callsites(callee.callee) flatMap {
      case (_, cs) =>
        // check 1: function is invoked
        if (isSamInvocation(cs))
          shouldInline = true

        var shouldPostInlineCs = false

        // check 2: function is passed as argument
        val forwardedAtIndices = cs.argInfos collect {
          case (i, ForwardedParam(`functionIndex`)) => i
        }
        val downstreamByForwarded: Iterable[Either[OptimizerWarning, InlineRequest]] = forwardedAtIndices flatMap { i =>
          inlineMegamorphicCallsite(cs, i) match {
            case Left(ws) => ws map Left.apply
            case Right(Some(request)) =>
              shouldInline = true
              shouldPostInlineCs = true
              List(Right(request))
            case Right(None) => Nil
          }
        }

        // check 3: value is captured
        val downstreamByCaptured = Nil // TODO

        val (warnings, csDownstreamRequests) = partitionLeftRight(downstreamByForwarded.toList ++ downstreamByCaptured)

        // even if there were some warnings, if we find a reason to inline cs, we return an inline request and discard the warnings
        if (shouldPostInlineCs) Some(Right(InlineRequest(cs, csDownstreamRequests)))
        else if (warnings.nonEmpty) warnings map Left.apply
        else None
    }

    val (warnings, downstreamRequests) = partitionLeftRight(downstream)

    if (shouldInline) Right(Some(InlineRequest(callsite, downstreamRequests)))
    else if (warnings.nonEmpty) Left(warnings)
    else Right(None)
  }

  /**
   * Note: boxes can be captured by methods or closures. For each of this case, we need to pass
   * along information saying what kind of operation that ultimately needs to be inlined.
   *   - for Ref: read and write the ref (remember what argument is the ref)
   *   - for Box: unbox and box operation (remember what argument is the box)
   *
   * Method capturing a Ref:
   *   def f = {
   *     var x = 0
   *     def g() { x += 1 }
   *     g()
   *     x
   *   }
   *
   * Closure capturing a Ref:
   *   def f(l: List[Int]) = {
   *     var x = 0
   *     for (i <- l) x += i
   *     x
   *   }
   *
   * Method capturing a box
   *   class C[T] { final def (x: T): T = x }
   *   def f(c: C[Int]) = c.f(10)
   *
   * Closure returning a box
   *   val f = (s: String) => s.length
   *   def t = f.apply("hi")
   */
//  def inlineEscapingBox(callsite: Callsite): Either[OptimizerWarning, Option[InlineRequest]] = {
//    null
//  }

  def inlineRequestForDefaultHeuristic(callsite: Callsite): Either[OptimizerWarning, Option[InlineRequest]] = {
    val requests: Iterable[Either[List[OptimizerWarning], Option[InlineRequest]]] = callsite.argInfos map {
      case (index, FunctionLiteral)   => inlineMegamorphicCallsite(callsite, index)
      case (index, ForwardedParam(_)) => inlineMegamorphicCallsite(callsite, index)
      //      case (index, CapturedBox) => inlineEscapingBox(callsite, index)
      case _ => Right(None)
    }
    mergeRequests(requests)
  }

  def mergeRequests(requests: Iterable[Either[List[OptimizerWarning], Option[InlineRequest]]]): Either[OptimizerWarning, Option[InlineRequest]] = {
    def merge(requests: List[InlineRequest]): InlineRequest = {
      val postByCs = requests.flatMap(_.post).groupBy(_.callsite)
      InlineRequest(requests.head.callsite, postByCs.valuesIterator.map(merge).toList)
    }

    val (warningOpts, inlineRequestOpts) = partitionLeftRight(requests)
    val inlineRequests = inlineRequestOpts.flatten
    if (inlineRequests.nonEmpty) Right(Some(merge(inlineRequests)))
    else {
      val warnings = warningOpts.flatten
      if (warnings.nonEmpty) Left(singleWarning(warnings))
      else Right(None)
    }
  }

  def singleWarning(warnings: List[OptimizerWarning]): OptimizerWarning = warnings.head

  /**
   * Returns the inline request for a callsite if the callsite should be inlined according to the
   * current heuristics (`-Yopt-inline-heuristics`).
   *
   * The resulting inline request may contain post-inlining requests of callsites that in turn are
   * also selected as individual inlining requests.
   *
   *
   * TODO: document why we need downstream inlining requests
   *   - some callistes can only be inlined as post because of additional knowledge
   *
   *
   * @return `None` if this callsite should not be inlined according to the active heuristic
   *         `Some(Left)` if the callsite cannot be inlined (for example because that would cause
   *           an IllegalAccessError) but should be according to the heuristic
   *           TODO: what if a downstream inline request would cause an IAE and we don't create an
   *           InlineRequest for the original callsite? new subclass of OptimizerWarning.
   *         `Some(Right)` if the callsite should be and can be inlined
   */
  def inlineRequest(callsite: Callsite): Option[Either[OptimizerWarning, InlineRequest]] = {
    val callee = callsite.callee.get
    def requestIfCanInline(callsite: Callsite): Either[OptimizerWarning, InlineRequest] = inliner.earlyCanInlineCheck(callsite) match {
      case Some(w) => Left(w)
      case None => Right(InlineRequest(callsite, Nil))
    }

    // TODO: also check downstream.
    def requestIfCanInlineR(inlineRequest: InlineRequest): Either[OptimizerWarning, InlineRequest] = inliner.earlyCanInlineCheck(inlineRequest.callsite) match {
      case Some(w) => Left(w)
      case None => Right(inlineRequest)
    }

    compilerSettings.YoptInlineHeuristics.value match {
      case "everything" =>
        if (callee.safeToInline) Some(requestIfCanInline(callsite))
        else None

      case "at-inline-annotated" =>
        if (callee.safeToInline && callee.annotatedInline) Some(requestIfCanInline(callsite))
        else None

      case "default" =>
        /**
         * TODO
         *  - move `canInline` test here. the heuristics may select various callsites for inlining
         *    with the idea that inlining X makes it valuable to inline Y. if inlining X fails,
         *    the inlining decision for Y should be made independently of X.
         *    in the end, keep only inlining requests for callsites that can be inlined.
         *  - extend the information in the callee. now it's just samParamTypes. for each samParamType,
         *    add information how it's used (if it's called or passed to another function)
         *      - called: prodCons, check final users, is it the receiver of a call
         *        ==> q: does it matter what function is called? it might well be not the SAM, for
         *            example a bridge that then invokes the sam.
         *      - passed to another function: only interesting if it's passed as a sam. for example,
         *        if a function is stored in a hash map, the parameter type of the "add" function is
         *        not a sam (it's probably Object). if the sam parameter is passed to another sam
         *        parameter, it's more likely that it will be called
         *        ==> should we actually check it?
         *  -
         *
         *  def fun = (x: Int) => x
         *  def bar = fun(10) // should inline fun, then eliminate the closure
         */
        if (callee.safeToInline && !callee.annotatedNoInline && !callsite.annotatedNoInline) {
          def shouldInlineHO = callee.samParamTypes.nonEmpty && (callee.samParamTypes exists {
            case (index, _) => callsite.argInfos.contains(index)
          })
          if (callee.annotatedInline || callsite.annotatedInline || shouldInlineHO) Some(requestIfCanInline(callsite))
          else None

          /*
          def paramType(index: Int) = ???
          def considerAsSamInvocation(invokedName: String, paramType: BType) = paramType match {
            case c: ClassBType =>
              val (samName, _) = c.info.get.inlineInfo.sam.get.span(_ != '(')
              invokedName.startsWith(samName)
            case _ => false
          }
          // TODO: for each samParam, use a list of actions - the param can be used in multiple ways
          // TODO: what if the callee doesn't call the function, but passes it as capture to another?
          //       def map(f) = { .. this.foreach(x => { ... f(x) .. } .. }
          //
          // TODO: could add information if the call is within a loop. but skip for now.
          //       - while loops are rare, function calls within while loops even more
          //       - usually loops are map / foreach etc. so if we consistently inline HOFs we are fine.
          //       - only danger: inline too much. a hof that doesn't call its arg in a loop. but that
          //         shouldn't hurt too much, we will also check method sizes.
          val isHOInvokingArg = callee.samParams exists {
            case (index, InvokeMethod(nameDesc)) =>
              val (name, _) = nameDesc.span(_ != '(')
              considerAsSamInvocation(name, paramType(index))
            case _ => false
          }
          */

          /*
          if (callee.annotatedInline || callee.annotatedInline) Some(requestIfCanInline(callsite))
          else {
            inlineRequestForDefaultHeuristic(callsite) match {
              case Left(w) => Some(Left(w))
              case Right(Some(req)) => Some(requestIfCanInlineR(req))
              case Right(None) => None
            }
          }
          */

//          val shouldInlineHO = callee.samParamTypes.nonEmpty && (callee.samParamTypes exists {
//            case (index, _) => callsite.argInfos.contains(index)
//          })
//          if (shouldInlineHO) Some(requestIfCanInline(callsite))
//          else None
        } else None
    }
  }

  /*
  // using http://lihaoyi.github.io/Ammonite/

  load.ivy("com.google.guava" % "guava" % "18.0")
  val javaUtilFunctionClasses = {
    val rt = System.getProperty("sun.boot.class.path").split(":").find(_.endsWith("lib/rt.jar")).get
    val u = new java.io.File(rt).toURL
    val l = new java.net.URLClassLoader(Array(u))
    val cp = com.google.common.reflect.ClassPath.from(l)
    cp.getTopLevelClasses("java.util.function").toArray.map(_.toString).toList
  }

  // found using IntelliJ's "Find Usages" on the @FunctionalInterface annotation
  val otherClasses = List(
    "com.sun.javafx.css.parser.Recognizer",
    "java.awt.KeyEventDispatcher",
    "java.awt.KeyEventPostProcessor",
    "java.io.FileFilter",
    "java.io.FilenameFilter",
    "java.lang.Runnable",
    "java.lang.Thread$UncaughtExceptionHandler",
    "java.nio.file.DirectoryStream$Filter",
    "java.nio.file.PathMatcher",
    "java.time.temporal.TemporalAdjuster",
    "java.time.temporal.TemporalQuery",
    "java.util.Comparator",
    "java.util.concurrent.Callable",
    "java.util.logging.Filter",
    "java.util.prefs.PreferenceChangeListener",
    "javafx.animation.Interpolatable",
    "javafx.beans.InvalidationListener",
    "javafx.beans.value.ChangeListener",
    "javafx.collections.ListChangeListener",
    "javafx.collections.MapChangeListener",
    "javafx.collections.SetChangeListener",
    "javafx.event.EventHandler",
    "javafx.util.Builder",
    "javafx.util.BuilderFactory",
    "javafx.util.Callback"
  )

  val allClasses = javaUtilFunctionClasses ::: otherClasses

  load.ivy("org.ow2.asm" % "asm" % "5.0.4")
  val classesAndSamNameDesc = allClasses.map(c => {
    val cls = Class.forName(c)
    val internalName = org.objectweb.asm.Type.getDescriptor(cls).drop(1).dropRight(1) // drop L and ;
    val sams = cls.getMethods.filter(m => {
      (m.getModifiers & java.lang.reflect.Modifier.ABSTRACT) != 0 &&
      m.getName != "equals" // Comparator has an abstract override of "equals" for adding Javadoc
    })
    assert(sams.size == 1, internalName + sams.map(_.getName))
    val sam = sams.head
    val samDesc = org.objectweb.asm.Type.getMethodDescriptor(sam)
    (internalName, sam.getName, samDesc)
  })
  println(classesAndSamNameDesc map {
    case (cls, nme, desc) => s"""("$cls", "$nme$desc")"""
  } mkString ("", ",\n", "\n"))
  */
  private val javaSams: Map[String, String] = Map(
    ("java/util/function/BiConsumer", "accept(Ljava/lang/Object;Ljava/lang/Object;)V"),
    ("java/util/function/BiFunction", "apply(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;"),
    ("java/util/function/BiPredicate", "test(Ljava/lang/Object;Ljava/lang/Object;)Z"),
    ("java/util/function/BinaryOperator", "apply(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;"),
    ("java/util/function/BooleanSupplier", "getAsBoolean()Z"),
    ("java/util/function/Consumer", "accept(Ljava/lang/Object;)V"),
    ("java/util/function/DoubleBinaryOperator", "applyAsDouble(DD)D"),
    ("java/util/function/DoubleConsumer", "accept(D)V"),
    ("java/util/function/DoubleFunction", "apply(D)Ljava/lang/Object;"),
    ("java/util/function/DoublePredicate", "test(D)Z"),
    ("java/util/function/DoubleSupplier", "getAsDouble()D"),
    ("java/util/function/DoubleToIntFunction", "applyAsInt(D)I"),
    ("java/util/function/DoubleToLongFunction", "applyAsLong(D)J"),
    ("java/util/function/DoubleUnaryOperator", "applyAsDouble(D)D"),
    ("java/util/function/Function", "apply(Ljava/lang/Object;)Ljava/lang/Object;"),
    ("java/util/function/IntBinaryOperator", "applyAsInt(II)I"),
    ("java/util/function/IntConsumer", "accept(I)V"),
    ("java/util/function/IntFunction", "apply(I)Ljava/lang/Object;"),
    ("java/util/function/IntPredicate", "test(I)Z"),
    ("java/util/function/IntSupplier", "getAsInt()I"),
    ("java/util/function/IntToDoubleFunction", "applyAsDouble(I)D"),
    ("java/util/function/IntToLongFunction", "applyAsLong(I)J"),
    ("java/util/function/IntUnaryOperator", "applyAsInt(I)I"),
    ("java/util/function/LongBinaryOperator", "applyAsLong(JJ)J"),
    ("java/util/function/LongConsumer", "accept(J)V"),
    ("java/util/function/LongFunction", "apply(J)Ljava/lang/Object;"),
    ("java/util/function/LongPredicate", "test(J)Z"),
    ("java/util/function/LongSupplier", "getAsLong()J"),
    ("java/util/function/LongToDoubleFunction", "applyAsDouble(J)D"),
    ("java/util/function/LongToIntFunction", "applyAsInt(J)I"),
    ("java/util/function/LongUnaryOperator", "applyAsLong(J)J"),
    ("java/util/function/ObjDoubleConsumer", "accept(Ljava/lang/Object;D)V"),
    ("java/util/function/ObjIntConsumer", "accept(Ljava/lang/Object;I)V"),
    ("java/util/function/ObjLongConsumer", "accept(Ljava/lang/Object;J)V"),
    ("java/util/function/Predicate", "test(Ljava/lang/Object;)Z"),
    ("java/util/function/Supplier", "get()Ljava/lang/Object;"),
    ("java/util/function/ToDoubleBiFunction", "applyAsDouble(Ljava/lang/Object;Ljava/lang/Object;)D"),
    ("java/util/function/ToDoubleFunction", "applyAsDouble(Ljava/lang/Object;)D"),
    ("java/util/function/ToIntBiFunction", "applyAsInt(Ljava/lang/Object;Ljava/lang/Object;)I"),
    ("java/util/function/ToIntFunction", "applyAsInt(Ljava/lang/Object;)I"),
    ("java/util/function/ToLongBiFunction", "applyAsLong(Ljava/lang/Object;Ljava/lang/Object;)J"),
    ("java/util/function/ToLongFunction", "applyAsLong(Ljava/lang/Object;)J"),
    ("java/util/function/UnaryOperator", "apply(Ljava/lang/Object;)Ljava/lang/Object;"),
    ("com/sun/javafx/css/parser/Recognizer", "recognize(I)Z"),
    ("java/awt/KeyEventDispatcher", "dispatchKeyEvent(Ljava/awt/event/KeyEvent;)Z"),
    ("java/awt/KeyEventPostProcessor", "postProcessKeyEvent(Ljava/awt/event/KeyEvent;)Z"),
    ("java/io/FileFilter", "accept(Ljava/io/File;)Z"),
    ("java/io/FilenameFilter", "accept(Ljava/io/File;Ljava/lang/String;)Z"),
    ("java/lang/Runnable", "run()V"),
    ("java/lang/Thread$UncaughtExceptionHandler", "uncaughtException(Ljava/lang/Thread;Ljava/lang/Throwable;)V"),
    ("java/nio/file/DirectoryStream$Filter", "accept(Ljava/lang/Object;)Z"),
    ("java/nio/file/PathMatcher", "matches(Ljava/nio/file/Path;)Z"),
    ("java/time/temporal/TemporalAdjuster", "adjustInto(Ljava/time/temporal/Temporal;)Ljava/time/temporal/Temporal;"),
    ("java/time/temporal/TemporalQuery", "queryFrom(Ljava/time/temporal/TemporalAccessor;)Ljava/lang/Object;"),
    ("java/util/Comparator", "compare(Ljava/lang/Object;Ljava/lang/Object;)I"),
    ("java/util/concurrent/Callable", "call()Ljava/lang/Object;"),
    ("java/util/logging/Filter", "isLoggable(Ljava/util/logging/LogRecord;)Z"),
    ("java/util/prefs/PreferenceChangeListener", "preferenceChange(Ljava/util/prefs/PreferenceChangeEvent;)V"),
    ("javafx/animation/Interpolatable", "interpolate(Ljava/lang/Object;D)Ljava/lang/Object;"),
    ("javafx/beans/InvalidationListener", "invalidated(Ljavafx/beans/Observable;)V"),
    ("javafx/beans/value/ChangeListener", "changed(Ljavafx/beans/value/ObservableValue;Ljava/lang/Object;Ljava/lang/Object;)V"),
    ("javafx/collections/ListChangeListener", "onChanged(Ljavafx/collections/ListChangeListener$Change;)V"),
    ("javafx/collections/MapChangeListener", "onChanged(Ljavafx/collections/MapChangeListener$Change;)V"),
    ("javafx/collections/SetChangeListener", "onChanged(Ljavafx/collections/SetChangeListener$Change;)V"),
    ("javafx/event/EventHandler", "handle(Ljavafx/event/Event;)V"),
    ("javafx/util/Builder", "build()Ljava/lang/Object;"),
    ("javafx/util/BuilderFactory", "getBuilder(Ljava/lang/Class;)Ljavafx/util/Builder;"),
    ("javafx/util/Callback", "call(Ljava/lang/Object;)Ljava/lang/Object;")
  )
  def javaSam(internalName: InternalName): Option[String] = javaSams.get(internalName)
}
