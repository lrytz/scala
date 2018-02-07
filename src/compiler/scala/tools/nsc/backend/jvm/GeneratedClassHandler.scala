package scala.tools.nsc
package backend.jvm

import java.nio.file.Path
import java.util.concurrent.ThreadPoolExecutor.CallerRunsPolicy
import java.util.concurrent._

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, Future}
import scala.reflect.internal.util.NoPosition
import scala.tools.nsc.backend.jvm.PostProcessorFrontendAccess.{BackendReporting, BufferingBackendReporting}
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.profile.ThreadPoolFactory
import scala.util.control.NonFatal

/**
 * Interface to handle post-processing and classfile writing (see [[PostProcessor]]) of generated
 * classes, potentially in parallel.
 */
private[jvm] sealed trait GeneratedClassHandler {
  val postProcessor: PostProcessor

  /**
    * Pass the result of code generation for a compilation unit to this handler for post-processing
    */
  def process(unit: GeneratedCompilationUnit)

  /**
   * If running in parallel, block until all generated classes are handled
   */
  def complete(): Unit

  /**
    * Invoked at the end of the jvm phase
    */
  def close(): Unit = ()
}

private[jvm] object GeneratedClassHandler {
  def apply(global: Global): GeneratedClassHandler = {
    import global._
    import genBCode.postProcessor

    val handler = settings.YaddBackendThreads.value match {
      case 1 =>
        new SyncWritingClassHandler(postProcessor)

      case maxThreads =>
        if (global.statistics.enabled)
          global.reporter.warning(global.NoPosition, "jvm statistics are not reliable with multi-threaded jvm class writing")
        val additionalThreads = maxThreads -1
        // the queue size is taken to be large enough to ensure that the a 'CallerRun' will not take longer to
        // run that it takes to exhaust the queue for the backend workers
        // when the queue is full, the main thread will no some background work
        // so this provides back-pressure
        val queueSize = if (settings.YmaxQueue.isSetByUser) settings.YmaxQueue.value else maxThreads * 2
        val threadPoolFactory = ThreadPoolFactory(global, currentRun.jvmPhase)
        val javaExecutor = threadPoolFactory.newBoundedQueueFixedThreadPool(additionalThreads, queueSize, new CallerRunsPolicy, "non-ast")
        val execInfo = ExecutorServiceInfo(additionalThreads, javaExecutor, javaExecutor.getQueue)
        new AsyncWritingClassHandler(postProcessor, execInfo)
    }

    if (settings.optInlinerEnabled || settings.optClosureInvocations)
      new GlobalOptimisingGeneratedClassHandler(postProcessor, handler)
    else handler
  }

  private class GlobalOptimisingGeneratedClassHandler(
      val postProcessor: PostProcessor,
      underlying: WritingClassHandler)
    extends GeneratedClassHandler {

    private val generatedUnits = ListBuffer.empty[GeneratedCompilationUnit]

    def process(unit: GeneratedCompilationUnit): Unit = generatedUnits += unit

    def complete(): Unit = {
      val allGeneratedUnits = generatedUnits.result()
      generatedUnits.clear()
      postProcessor.runGlobalOptimizations(allGeneratedUnits)
      allGeneratedUnits.foreach(underlying.process)
      underlying.complete()
    }

    override def close(): Unit = underlying.close()

    override def toString: String = s"GloballyOptimising[$underlying]"
  }

  sealed abstract class WritingClassHandler(val javaExecutor: Executor) extends GeneratedClassHandler {
    import postProcessor.bTypes.frontendAccess

    def tryStealing: Option[Runnable]

    private val processingUnits = ListBuffer.empty[CompilationUnitInPostProcess]

    def process(unit: GeneratedCompilationUnit): Unit = {
      val paths = CompilationUnitPaths(unit.sourceFile, frontendAccess.compilerSettings.outputDirectory(unit.sourceFile))
      val unitInPostProcess = new CompilationUnitInPostProcess(unit.classes, paths)
      postProcessUnit(unitInPostProcess)
      processingUnits += unitInPostProcess
    }

    protected implicit val executionContext: ExecutionContextExecutor = ExecutionContext.fromExecutor(javaExecutor)

    final def postProcessUnit(unitInPostProcess: CompilationUnitInPostProcess): Unit = {
      unitInPostProcess.task = Future {
        frontendAccess.withThreadLocalReporter(unitInPostProcess.bufferedReporting) {
          // we 'take' classes to reduce the memory pressure
          // as soon as the class is consumed and written, we release its data
          unitInPostProcess.takeClasses() foreach {
            postProcessor.sendToDisk(_, unitInPostProcess.paths)
          }
        }
      }
    }

    protected def getAndClearProcessingUnits(): List[CompilationUnitInPostProcess] = {
      val result = processingUnits.result()
      processingUnits.clear()
      result
    }

    override def complete(): Unit = {
      import frontendAccess.directBackendReporting

      def stealWhileWaiting(unitInPostProcess: CompilationUnitInPostProcess, fut: Future[Unit]): Unit = {
        while (!fut.isCompleted)
          tryStealing match {
            case Some(r) => r.run()
            case None => Await.ready(fut, Duration.Inf)
        }
        //we know that they are complete by we need to check for exception
        //but first get any reports
        unitInPostProcess.bufferedReporting.relayReports(directBackendReporting)
        fut.value.get.get // throw the exception if the future completed with a failure
      }


      /** We could consume the results when yey are ready, via use of a [[java.util.concurrent.CompletionService]]
        * or something similar, but that would lead to non deterministic reports from backend threads, as the
        * compilation unit could complete in a different order that when they were submitted, and thus the relayed
        * reports would be in a different order.
        * To avoid that non-determinism we read the result in order or submission, with a potential minimal performance
        * loss, do to the memory being retained longer for tasks that it might otherwise.
        * Most of the memory in the CompilationUnitInPostProcess is reclaimable anyway as the classes are deferenced after use
        */
      getAndClearProcessingUnits().foreach { unitInPostProcess =>
        try {
          stealWhileWaiting(unitInPostProcess, unitInPostProcess.task)
        } catch {
          case NonFatal(t) =>
            t.printStackTrace()
            frontendAccess.backendReporting.error(NoPosition, s"unable to write ${unitInPostProcess.paths.sourceFile} $t")
        }
      }
    }
  }

  private final class SyncWritingClassHandler(val postProcessor: PostProcessor) extends WritingClassHandler((r) => r.run()) {
    override def toString: String = s"SyncWriting"

    override def tryStealing: Option[Runnable] = None
  }

  private final case class ExecutorServiceInfo(maxThreads: Int, javaExecutor: ExecutorService, queue: BlockingQueue[Runnable])

  private final class AsyncWritingClassHandler(val postProcessor: PostProcessor,
                                               val executorServiceInfo: ExecutorServiceInfo)
    extends WritingClassHandler(executorServiceInfo.javaExecutor) {

    override def toString: String = s"AsyncWriting[additional threads:${executorServiceInfo.maxThreads}]"

    override def close(): Unit = {
      super.close()
      executorServiceInfo.javaExecutor.shutdownNow()
    }

    override def tryStealing: Option[Runnable] = Option(executorServiceInfo.queue.poll())
  }

}

/** Paths for a compilation unit, used during classfile writing */
final case class CompilationUnitPaths(sourceFile: AbstractFile, outputDir: AbstractFile) {
  def outputPath: Path = outputDir.file.toPath // `toPath` caches its result
}

/**
 * State for a compilation unit being post-processed.
 *   - Holds the classes to post-process (released for GC when no longer used)
 *   - Keeps a reference to the future that runs the post-processor
 *   - Buffers messages reported during post-processing
 */
final class CompilationUnitInPostProcess(private var classes: List[GeneratedClass], val paths: CompilationUnitPaths) {
  def takeClasses(): List[GeneratedClass] = {
    val c = classes
    classes = Nil
    c
  }

  /** the main async task submitted onto the scheduler */
  var task: Future[Unit] = _

  val bufferedReporting = new BufferingBackendReporting
}
