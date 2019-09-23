package scala.tools.nsc.tasty

import scala.reflect.ClassTag

object TastyRefs {

  /** An address pointing to an index in a Tasty buffer's byte array */
  case class Addr(index: Int) extends AnyVal {
    def - (delta: Int): Addr = Addr(this.index - delta)
    def + (delta: Int): Addr = Addr(this.index + delta)

    def relativeTo(base: Addr): Addr = this - base.index - AddrWidth

    def ==(that: Addr): Boolean = this.index == that.index
    def !=(that: Addr): Boolean = this.index != that.index
  }

  val NoAddr: Addr = Addr(-1)

  /** The maximal number of address bytes.
   *  Since addresses are written as base-128 natural numbers,
   *  the value of 4 gives a maximal array size of 256M.
   */
  final val AddrWidth = 4

  /** An address referring to a serialized name */
  case class NameRef(index: Int) extends AnyVal
}