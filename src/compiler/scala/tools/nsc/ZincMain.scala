/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools
package nsc

import xsbti.{ AnalysisCallback, Logger, Problem, Reporter }
import xsbti.compile._
import java.util.function.Supplier
import reporters.ZincDelegatingReporter

class ZincMainClass extends Driver with EvalLoop with java.io.Closeable {
  var compiler: ZincGlobal = _

  def resident(compiler: Global): Unit = loop { line =>
    val command = new CompilerCommand(line.split("\\s+").toList, new Settings(scalacError))
    compiler.reporter.reset()
    new compiler.Run() compile command.files
  }

  override def newCompiler(): Global = Global(settings)
  def newCompiler(settings: Settings, reporter: ZincDelegatingReporter, output: Output): ZincGlobal =
    new ZincGlobal(settings, reporter, output)

  override def doCompile(compiler: Global): Unit = {
    if (settings.resident) resident(compiler)
    else super.doCompile(compiler)
  }

  def noErrors(reporter: ZincDelegatingReporter): Boolean = !reporter.hasErrors && command.ok

  def process(
    args: Array[String],
    changes: DependencyChanges,
    callback: AnalysisCallback,
    bridgeCallback: ZincMainCallback,
    log: Logger,
    delegate: Reporter,
    progress: CompileProgress,
    output: Output): Boolean = {
    val initialLog = new WeakLog(log, delegate)
    settings = new Settings(msg => initialLog(msg))
    command = new CompilerCommand(args.toList, settings)
    val dreporter = ZincDelegatingReporter(settings, initialLog.reporter)
    reporter = dreporter

    try {
      if (!noErrors(dreporter)) {
        dreporter.printSummary()
        handleErrors(dreporter, initialLog.logger)
      }
    } finally initialLog.clear()
    def handleErrors(dreporter: ZincDelegatingReporter, log: Logger): Unit = {
      debug(log, "Compilation failed (CompilerInterface)")
      bridgeCallback.handleErrors(args, dreporter.problems)
    }
    compiler = newCompiler(command.settings, dreporter, output)
    if (command.shouldStopWithInfo) {
      dreporter.info(null, command.getInfoMessage(compiler), true)
      bridgeCallback.handleErrors(args, Array())
    }

    if (noErrors(dreporter)) {
      compiler.set(callback, dreporter)
      val compiler0 = compiler
      val run = new compiler0.ZincRun(progress)
      run.compile(command.files)
      // processUnreportedWarnings(run)
      // dreporter.problems.foreach(
      //   p => callback.problem(p.category, p.position, p.message, p.severity, true)
      // )
    }

    // the case where we cancelled compilation _after_ some compilation errors got reported
    // will be handled by line above so errors still will be reported properly just potentially not
    // all of them (because we cancelled the compilation)
    // if (dreporter.cancelled) {
    //   bridgeCallback.handleCompilationCancellation(dreporter, log)
    // }
    noErrors(dreporter)
  }

  def close(): Unit = {
    Option(compiler) match {
      case Some(c: java.io.Closeable) => c.close()
      case _                          =>
    }
  }

  def debug(log: Logger, msg: => String): Unit = log.debug(message(msg))

  private final class WeakLog(private[this] var log: Logger, private[this] var delegate: Reporter) {
    import scala.jdk.FunctionConverters._
    def apply(msg: => String): Unit = {
      assert(log ne null, "Stale reference to logger")
      log.error(message(msg))
    }
    def logger: Logger = log
    def reporter: Reporter = delegate
    def clear(): Unit = {
      log = null
      delegate = null
    }
  }

  def message(message: => String): Supplier[String] = {
    import scala.jdk.FunctionConverters._
    (() => message).asJava
  }
}

object ZincMain extends ZincMainClass { }

trait ZincMainCallback {
  def handleErrors(args: Array[String], problems: Array[Problem]): Unit
  def handleCompilationCancellation(args: Array[String], problems: Array[Problem]): Unit
}
