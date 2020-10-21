package app {
  import scala.concurrent.{Batchable, ExecutionContextExecutor, TheBatchingExecutor, ThePromiseTransformation}

  object batchingGlobal extends ExecutionContextExecutor with TheBatchingExecutor {
    import scala.concurrent.ExecutionContext.global

    final override def submitForExecution(runnable: Runnable): Unit = global.execute(runnable)

    final override def execute(runnable: Runnable): Unit =
      if ((!runnable.isInstanceOf[ThePromiseTransformation[_, _]] || runnable.asInstanceOf[ThePromiseTransformation[_, _]].benefitsFromBatching) && runnable.isInstanceOf[Batchable])
        submitAsyncBatched(runnable)
      else
        submitForExecution(runnable)

    override final def reportFailure(t: Throwable): Unit = global.reportFailure(t)
  }
}
