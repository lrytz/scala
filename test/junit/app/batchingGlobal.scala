package scala.concurrent {
  object A {
    type BatchingExecutor = scala.concurrent.BatchingExecutor
    type PromiseTransformation[-F, T] = scala.concurrent.impl.Promise.Transformation[F, T]
  }
}

package app {
  import scala.concurrent.{A, Batchable, ExecutionContextExecutor}

  object batchingGlobal extends ExecutionContextExecutor with A.BatchingExecutor {
    import scala.concurrent.ExecutionContext.global

    final override def submitForExecution(runnable: Runnable): Unit = global.execute(runnable)

    final override def execute(runnable: Runnable): Unit =
      if ((!runnable.isInstanceOf[A.PromiseTransformation[_, _]] || runnable.asInstanceOf[A.PromiseTransformation[_, _]].benefitsFromBatching) && runnable.isInstanceOf[Batchable])
        submitAsyncBatched(runnable)
      else
        submitForExecution(runnable)

    override final def reportFailure(t: Throwable): Unit = global.reportFailure(t)
  }
}
