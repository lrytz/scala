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

package scala.tools.nsc.tasty

import scala.collection.mutable

import scala.tools.tasty.{TastyFormat, TastyRefs, TastyReader, TastyName, ErasedTypeRef, Signature, TastyHeaderUnpickler}
import TastyFormat.NameTags._, TastyRefs.NameRef, TastyName._

object TastyUnpickler {

  abstract class SectionUnpickler[R](val name: String) {
    def unpickle(reader: TastyReader, nameTable: NameRef => TastyName): R
  }

  final class Table[T] extends (NameRef => T) {
    private[this] val names = new mutable.ArrayBuffer[T]
    def add(name: T): mutable.ArrayBuffer[T] = names += name
    def apply(ref: NameRef): T = names(ref.index)
    def size: Int = names.size
  }
}

import TastyUnpickler._

class TastyUnpickler[Tasty <: TastyUniverse](reader: TastyReader)(implicit tasty: Tasty) { self =>
  import tasty.{Context, assert}
  import reader._

  def this(bytes: Array[Byte])(implicit tasty: Tasty) = this(new TastyReader(bytes))

  private val sectionReader = new mutable.HashMap[String, TastyReader]

  val nameAtRef = new Table[TastyName]

  private def readName(): TastyName = nameAtRef(readNameRef())

  private def readParamSig(): Signature.ParamSig[ErasedTypeRef] = {
    val ref = readInt()
    if (ref < 0)
      Left(ref.abs)
    else {
      Right(ErasedTypeRef(nameAtRef(NameRef(ref))))
    }
  }

  private def readNameContents()(implicit ctx: Context): TastyName = {
    val tag = readByte()
    val length = readNat()
    val start = currentAddr
    val end = start + length
    def debugName(name: TastyName): name.type = {
      ctx.log(s"${nameAtRef.size}: ${name.debug}")
      name
    }
    val result = tag match {
      case UTF8 =>
        goto(end)
        debugName(SimpleName(new String(bytes.slice(start.index, start.index + length), "UTF-8")))
      case tag @ (QUALIFIED | EXPANDED | EXPANDPREFIX) =>
        val sep = tag match {
          case QUALIFIED    => TastyName.PathSep
          case EXPANDED     => TastyName.ExpandedSep
          case EXPANDPREFIX => TastyName.ExpandPrefixSep
        }
        debugName(QualifiedName(readName(), sep, readName().asSimpleName))
      case UNIQUE =>
        val separator = readName().asSimpleName
        val num       = readNat()
        val originals = until(end)(readName())
        val original = if (originals.isEmpty) TastyName.Empty else originals.head
        debugName(UniqueName(original, separator, num))
      case DEFAULTGETTER =>
        debugName(DefaultName(readName(), readNat()))
      case SIGNED =>
        val original  = readName()
        val result    = ErasedTypeRef(readName())
        val paramsSig = until(end)(readParamSig())
        val sig       = Signature(paramsSig, result)
        debugName(SignedName(original, sig))
      case OBJECTCLASS =>
        debugName(ObjectName(readName()))
      case INLINEACCESSOR | SUPERACCESSOR =>
        val prefix = tag match {
          case INLINEACCESSOR => TastyName.InlinePrefix
          case SUPERACCESSOR  => TastyName.SuperPrefix
        }
        debugName(PrefixName(prefix, readName()))
      case _ =>
        val qual = readName()
        sys.error(s"at NameRef(${nameAtRef.size}): name `${qual.debug}` is qualified by unknown tag $tag")
    }
    assert(currentAddr == end, s"bad name ${result.debug} $start $currentAddr $end")
    result
  }

  new TastyHeaderUnpickler(reader).readHeader()

  def readSections()(implicit ctx: Context): Unit = {
    ctx.log(s"reading names:")
    doUntil(readEnd()) { nameAtRef.add(readNameContents()) }
    while (!isAtEnd) {
      val secName = readName().asSimpleName.raw
      val secEnd = readEnd()
      sectionReader(secName) = new TastyReader(bytes, currentAddr.index, secEnd.index, currentAddr.index)
      goto(secEnd)
    }
  }

  def unpickle[R](sec: SectionUnpickler[R]): Option[R] =
    for (reader <- sectionReader.get(sec.name)) yield
      sec.unpickle(reader, nameAtRef)

  private[nsc] def bytes: Array[Byte] = reader.bytes
}
