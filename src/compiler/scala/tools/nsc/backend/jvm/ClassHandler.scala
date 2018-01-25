package scala.tools.nsc.backend.jvm

import java.util.concurrent.ThreadPoolExecutor.CallerRunsPolicy
import java.util.concurrent._

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContextExecutor, ExecutionContext, Future, Promise}
import scala.reflect.internal.util.{NoPosition, Position, SourceFile}
import scala.tools.nsc.Settings
import scala.tools.nsc.backend.jvm.PostProcessorFrontendAccess.BackendReporting
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.profile.AsyncHelper
import scala.util.control.NonFatal

private[jvm] sealed trait ClassHandler {
  def close(): Unit

  def complete(): Unit

  def endUnit(unit:SourceFile)

  def startProcess(clazz: GeneratedClass): Unit

  def initialise(): Unit = ()

  val postProcessor:PostProcessor
}

private[jvm] object ClassHandler {

  def apply(asyncHelper: AsyncHelper, cfWriter: ClassfileWriter, settings:Settings, postProcessor: PostProcessor) = {
    val unitInfoLookup = settings.outputDirs.getSingleOutput match {
      case Some(dir) => new SingleUnitInfo(postProcessor.bTypes.frontendAccess, dir)
      case None => new LookupUnitInfo(postProcessor.bTypes.frontendAccess)
    }
    val writer = settings.YaddBackendThreads.value match {
      case 1 => new SyncWritingClassHandler(unitInfoLookup, postProcessor, cfWriter)
      case maxThreads =>
        val additionalThreads = maxThreads -1
        // the queue size is taken to be large enough to ensure that the a 'CallerRun' will not take longer to
        // run that it takes to exhaust the queue for the backend workers
        // when the queue is full, the main thread will no some background work
        // so this provides back-pressure
        val queueSize = if (settings.YmaxQueue.isSetByUser) settings.YmaxQueue.value else maxThreads * 2
        val javaExecutor = asyncHelper.newBoundedQueueFixedThreadPool(additionalThreads, queueSize, new CallerRunsPolicy, "non-ast")
        val execInfo = ExecutorServiceInfo(additionalThreads, javaExecutor, javaExecutor.getQueue)
        new AsyncWritingClassHandler(unitInfoLookup, postProcessor, cfWriter, execInfo)
    }

    if (settings.optInlinerEnabled || settings.optClosureInvocations)
      new GlobalOptimisingGeneratedClassHandler(postProcessor, writer)
    else writer
  }

  private class GlobalOptimisingGeneratedClassHandler(val postProcessor: PostProcessor, val underlying: WritingClassHandler) extends ClassHandler {
    private val bufferBuilder = List.newBuilder[GeneratedClass]

    override def close(): Unit = underlying.close()

    override def startProcess(clazz: GeneratedClass): Unit = bufferBuilder += clazz

    override def complete(): Unit = {
      globalOptimise()
      underlying.complete()
    }

    override def endUnit(unit: SourceFile): Unit = ()

    private def globalOptimise(): Unit = {
      val allClasses = bufferBuilder.result()
      postProcessor.runGlobalOptimizations(allClasses)

      //replay the units to underlying
      var current = allClasses.head
      underlying.startProcess(current)
      for (next <- allClasses.tail) {
        if (next.sourceFile ne current.sourceFile) {
          underlying.endUnit(current.sourceFile)
        }
        underlying.startProcess(next)
        current = next
      }
      underlying.endUnit(current.sourceFile)
    }

    override def initialise(): Unit = {
      bufferBuilder.clear()
      underlying.initialise()
    }

    override def toString: String = s"GloballyOptimising[$underlying]"
  }

  sealed abstract class WritingClassHandler(val javaExecutor :Executor) extends ClassHandler{
    val unitInfoLookup: UnitInfoLookup
    val cfWriter: ClassfileWriter

    protected val pendingBuilder = List.newBuilder[UnitResult]
    private val inUnit = ListBuffer.empty[GeneratedClass]

    override def startProcess(clazz: GeneratedClass): Unit = {
      inUnit += clazz
    }

    protected def pending(): List[UnitResult] = {
      val result = pendingBuilder.result()
      pendingBuilder.clear()
      result
    }
    override def initialise(): Unit = {
      super.initialise()
      pendingBuilder.clear()
    }

    override def endUnit(unit:SourceFile): Unit = {
      val unitProcess = new UnitResult(unitInfoLookup, inUnit.result, unit)
      inUnit.clear()
      postProcessUnit(unitProcess)

      pendingBuilder += unitProcess
    }

    protected implicit val executionContext: ExecutionContextExecutor = ExecutionContext.fromExecutor(javaExecutor)

    final def postProcessUnit(unitProcess: UnitResult): Unit = {
      unitProcess.task = Future {
        // we 'take' classes to reduce the memory pressure
        // as soon as the class is consumed and written, we release its data
        unitProcess.takeClasses foreach {
          postProcessor.sendToDisk(unitProcess, _, cfWriter)
        }
        unitProcess.completedUnit()
      }
    }

    override def close(): Unit = cfWriter.close()

    override def complete(): Unit = {
      val directBackendReporting = postProcessor.bTypes.frontendAccess.directBackendReporting
      def stealWhileWaiting(unitResult: UnitResult, fut: Future[Unit]): Unit = {
        while (!fut.isCompleted)
          tryStealing match {
            case Some(r:Runnable) => r.run()
            case None => Await.ready(fut, Duration.Inf)
        }
        //we know that they are complete by we need to check for exception
        //but first get any reports
        unitResult.relayReports(directBackendReporting)
        fut.value.get.get
      }

      // This way it is easier to test, as the results are deterministic
      // the the loss of potential performance is probably minimal
      pending().foreach {
        unitResult: UnitResult =>
          try {
            stealWhileWaiting(unitResult, unitResult.task)
            stealWhileWaiting(unitResult, unitResult.result.future)
          } catch {
            case NonFatal(t) =>
              t.printStackTrace()
              postProcessor.bTypes.frontendAccess.backendReporting.error(NoPosition, s"unable to write ${unitResult.source} $t")
          }
      }
    }
    def tryStealing:Option[Runnable]
  }
  private final class SyncWritingClassHandler(val unitInfoLookup: UnitInfoLookup,
                val postProcessor: PostProcessor, val cfWriter: ClassfileWriter)
    extends WritingClassHandler((r) => r.run()) {

    override def toString: String = s"SyncWriting [$cfWriter]"

    override def tryStealing: Option[Runnable] = None
  }
  private final case class ExecutorServiceInfo( maxThreads:Int, javaExecutor : ExecutorService, queue:BlockingQueue[Runnable])

  private final class AsyncWritingClassHandler(val unitInfoLookup: UnitInfoLookup,
                                               val postProcessor: PostProcessor,
                                               val cfWriter: ClassfileWriter,
                                               val executorServiceInfo: ExecutorServiceInfo)
    extends WritingClassHandler(executorServiceInfo.javaExecutor) {

    override def toString: String = s"AsyncWriting[additional threads:${executorServiceInfo.maxThreads} writer:$cfWriter]"

    override def close(): Unit = {
      super.close()
      executorServiceInfo.javaExecutor.shutdownNow()
    }

    override def tryStealing: Option[Runnable] = Option(executorServiceInfo.queue.poll())
  }

}
//we avoid the lock on frontendSync for the common case, when compiling to a single target
sealed trait UnitInfoLookup {
  def outputDir(source:AbstractFile) : AbstractFile
  val frontendAccess: PostProcessorFrontendAccess
}
final class SingleUnitInfo(val frontendAccess: PostProcessorFrontendAccess, constantOutputDir:AbstractFile) extends UnitInfoLookup {
  override def outputDir(source: AbstractFile) = constantOutputDir
}
final class LookupUnitInfo(val frontendAccess: PostProcessorFrontendAccess) extends UnitInfoLookup {
  lazy val outputDirectories = frontendAccess.compilerSettings.outputDirectories
  override def outputDir(source: AbstractFile) = outputDirectories.outputDirFor(source)
}
sealed trait SourceUnit {
  def withBufferedReporter[T](fn: => T): T

  val outputDir: AbstractFile
  val outputPath: java.nio.file.Path
  def sourceFile:AbstractFile

}
final class UnitResult(unitInfoLookup: UnitInfoLookup, classes_ : List[GeneratedClass], val source:SourceFile) extends SourceUnit with BackendReporting {
  lazy val outputDir = unitInfoLookup.outputDir(source.file)

  override def sourceFile = source.file

  lazy val outputPath = outputDir.file.toPath
  private var classes: List[GeneratedClass] = classes_

  def copyClasses = classes

  def takeClasses(): List[GeneratedClass] = {
    val c = classes
    classes = Nil
    c
  }

  /** the main async task submitted onto the scheduler */
  var task: Future[Unit] = _

  /** the final completion which may occur after the task completes
    * this allows the use of async completions */
  val result = Promise[Unit]()

  def completedUnit(): Unit = result.trySuccess(())

  def relayReports(backendReporting: BackendReporting): Unit = this.synchronized {
    if (bufferedReports nonEmpty) {
      for (report: Report <- bufferedReports.reverse) {
        report.relay(backendReporting)
      }
    }
  }

  private var bufferedReports = List.empty[Report]

  override def withBufferedReporter[T](fn: => T) = unitInfoLookup.frontendAccess.withLocalReporter(this)(fn)

  override def inlinerWarning(pos: Position, message: String): Unit =
    this.synchronized(bufferedReports ::= new ReportInlinerWarning(pos, message))

  override def error(pos: Position, message: String): Unit =
    this.synchronized(bufferedReports ::= new ReportError(pos, message))

  override def warning(pos: Position, message: String): Unit =
    this.synchronized(bufferedReports ::= new ReportWarning(pos, message))

  override def inform(message: String): Unit =
    this.synchronized(bufferedReports ::= new ReportInform(message))

  override def log(message: String): Unit =
    this.synchronized(bufferedReports ::= new ReportLog(message))

  private sealed trait Report {
    def relay(backendReporting: BackendReporting): Unit
  }

  private class ReportInlinerWarning(pos: Position, message: String) extends Report {
    override def relay(reporting: BackendReporting): Unit =
      reporting.inlinerWarning(pos, message)
  }

  private class ReportError(pos: Position, message: String) extends Report {
    override def relay(reporting: BackendReporting): Unit =
      reporting.error(pos, message)
  }

  private class ReportWarning(pos: Position, message: String) extends Report {
    override def relay(reporting: BackendReporting): Unit =
      reporting.warning(pos, message)
  }

  private class ReportInform(message: String) extends Report {
    override def relay(reporting: BackendReporting): Unit =
      reporting.inform(message)
  }

  private class ReportLog(message: String) extends Report {
    override def relay(reporting: BackendReporting): Unit =
      reporting.log(message)
  }

}

