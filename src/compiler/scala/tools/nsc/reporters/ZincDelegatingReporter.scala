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

package scala
package tools.nsc
package reporters

import java.io.File
import java.util.Optional
import scala.reflect.internal.util.{ FakePos, NoPosition, Position }
import scala.tools.nsc.Settings
import scala.jdk.OptionConverters._
import xsbti.{
  Problem => XProblem,
  Position => XPosition,
  Severity => XSeverity,
  Reporter => XReporter
}

object ZincDelegatingReporter {
  def apply(settings: Settings, delegate: XReporter): ZincDelegatingReporter =
    new ZincDelegatingReporter(
      settings.fatalWarnings.value,
      settings.nowarn.value,
      delegate)

  class PositionImpl(
    sourcePath0: Option[String],
    sourceFile0: Option[File],
    line0: Option[Int],
    lineContent0: String,
    offset0: Option[Int],
    pointer0: Option[Int],
    pointerSpace0: Option[String],
    startOffset0: Option[Int],
    endOffset0: Option[Int],
    startLine0: Option[Int],
    startColumn0: Option[Int],
    endLine0: Option[Int],
    endColumn0: Option[Int])
      extends XPosition {
    override val line: Optional[Integer] = o2oi(line0)
    override val lineContent = lineContent0
    override val offset: Optional[Integer] = o2oi(offset0)
    override val sourcePath = sourcePath0.toJava
    override val sourceFile = sourceFile0.toJava
    override val pointer: Optional[Integer] = o2oi(pointer0)
    override val pointerSpace = pointerSpace0.toJava
    override val startOffset: Optional[Integer] = o2oi(startOffset0)
    override val endOffset: Optional[Integer] = o2oi(endOffset0)
    override val startLine: Optional[Integer] = o2oi(startLine0)
    override val startColumn: Optional[Integer] = o2oi(startColumn0)
    override val endLine: Optional[Integer] = o2oi(endLine0)
    override val endColumn: Optional[Integer] = o2oi(endColumn0)
    override def toString =
      (sourcePath0, line0) match {
        case (Some(s), Some(l)) => s + ":" + l
        case (Some(s), _)       => s + ":"
        case _                  => ""
      }
  }

  object PositionImpl {
    def empty: PositionImpl =
      new PositionImpl(None, None, None, "", None, None, None, None, None, None, None, None, None)
  }

  private[nsc] def o2oi(opt: Option[Int]): Optional[java.lang.Integer] = {
    opt match {
      case Some(s) => Optional.ofNullable[java.lang.Integer](s: java.lang.Integer)
      case None    => Optional.empty[java.lang.Integer]
    }
  }

  private[nsc] def convert(dirtyPos: Position): xsbti.Position = {
    def cleanPos(pos: Position) = {
      Option(pos) match {
        case None | Some(NoPosition) => None
        case Some(_: FakePos)        => None
        case _                       => Option(pos.finalPosition)
      }
    }

    def makePosition(pos: Position): xsbti.Position = {
      val src = pos.source
      val sourcePath = src.file.path
      val sourceFile = src.file.file
      val line = pos.line
      val lineContent = pos.lineContent.stripLineEnd
      val offset = pos.point

      // Same logic as Position#line
      def lineOf(offset: Int) = src.offsetToLine(offset) + 1
      def columnOf(offset: Int) = offset - src.lineToOffset(src.offsetToLine(offset))

      val pointer = columnOf(offset)
      val pointerSpace = lineContent.toList.take(pointer).map {
        case '\t' => '\t'
        case _    => ' '
      }

      val startOffset = if (pos.isRange) Some(pos.start) else None
      val endOffset = if (pos.isRange) Some(pos.end) else None
      val startLine = if (pos.isRange) Some(lineOf(pos.start)) else None
      val startColumn = if (pos.isRange) Some(columnOf(pos.start)) else None
      val endLine = if (pos.isRange) Some(lineOf(pos.end)) else None
      val endColumn = if (pos.isRange) Some(columnOf(pos.end)) else None

      new PositionImpl(
        Option(sourcePath),
        Option(sourceFile),
        Option(line),
        lineContent,
        Option(offset),
        Option(pointer),
        Option(pointerSpace.mkString),
        startOffset,
        endOffset,
        startLine,
        startColumn,
        endLine,
        endColumn
      )
    }

    cleanPos(dirtyPos) match {
      case None           => PositionImpl.empty
      case Some(cleanPos) => makePosition(cleanPos)
    }
  }
}

final class ZincDelegatingReporter(
    warnFatal: Boolean,
    noWarn: Boolean,
    private[this] var delegate: XReporter
) extends Reporter {
  def dropDelegate(): Unit = { delegate = null }
  def error(msg: String): Unit = error(FakePos("scalac"), msg)
  def printSummary(): Unit = delegate.printSummary()

  def problems = delegate.problems
  override def hasErrors = delegate.hasErrors
  override def hasWarnings = delegate.hasWarnings
  override def comment(pos: Position, msg: String): Unit =
    delegate.comment(ZincDelegatingReporter.convert(pos), msg)
  override def reset(): Unit = {
    super.reset()
    delegate.reset()
  }

  protected def info0(pos: Position, msg: String, rawSeverity: Severity, force: Boolean): Unit = {
    val skip = rawSeverity == WARNING && noWarn
    if (!skip) {
      val severity = if (warnFatal && rawSeverity == WARNING) ERROR else rawSeverity
      delegate.log(new CompileProblem(ZincDelegatingReporter.convert(pos), msg, convert(severity)))
    }
  }

  import xsbti.Severity.{ Info, Warn, Error }
  private[this] def convert(sev: Severity): XSeverity = {
    sev match {
      case INFO    => Info
      case WARNING => Warn
      case ERROR   => Error
    }
  }

  private final class CompileProblem(
      pos: XPosition,
      msg: String,
      sev: XSeverity
  ) extends XProblem {
    override val category = ""
    override val position = pos
    override val message = msg
    override val severity = sev
    override def toString = s"[$severity] $pos: $message"
  }
}
