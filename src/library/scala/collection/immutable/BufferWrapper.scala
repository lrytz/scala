package scala.collection.immutable

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{SeqFactory, StrictOptimizedSeqFactory, mutable}

final class BufferWrapper[+A](buf: mutable.Buffer[A]) extends AbstractSeq[A]
  with IndexedSeq[A]
  with IndexedSeqOps[A, ArraySeq, BufferWrapper[A]]
  with StrictOptimizedSeqOps[A, ArraySeq, BufferWrapper[A]]
  with Serializable {

  override protected def fromSpecific(coll: IterableOnce[A @uncheckedVariance]): BufferWrapper[A] = BufferWrapper.from(coll)
  override protected def newSpecificBuilder: mutable.Builder[A @uncheckedVariance, BufferWrapper[A @uncheckedVariance]] = BufferWrapper.newBuilder
  override def empty: BufferWrapper[A] = BufferWrapper.empty

  override def iterableFactory: SeqFactory[ArraySeq] = ArraySeqGenericFactory

  def apply(i: Int): A = buf(i)

  def length: Int = buf.length

  override protected[this] def className: String = "BufferWrapper"
}

object ArraySeqGenericFactory extends StrictOptimizedSeqFactory[ArraySeq] {
  override def from[A](source: IterableOnce[A]): ArraySeq[A] = ArraySeq.unsafeWrapArray(Array.from[Any](source)).asInstanceOf[ArraySeq[A]]

  override def empty[A]: ArraySeq[A] = ArraySeq.empty

  override def newBuilder[A]: mutable.Builder[A, ArraySeq[A]] = ArraySeq.newBuilder[Any].mapResult(_.asInstanceOf[ArraySeq[A]])
}


object BufferWrapper extends StrictOptimizedSeqFactory[BufferWrapper] {
  override def from[A](source: IterableOnce[A]): BufferWrapper[A] = new BufferWrapper(mutable.Buffer.from(source))

  override def empty[A]: BufferWrapper[A] = new BufferWrapper[A](mutable.Buffer.empty)

  override def newBuilder[A]: mutable.Builder[A, BufferWrapper[A]] = mutable.Buffer.newBuilder[A].mapResult(new BufferWrapper(_))
}
