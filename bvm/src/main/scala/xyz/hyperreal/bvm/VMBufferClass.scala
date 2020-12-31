package xyz.hyperreal.bvm

import scala.collection.mutable.ArrayBuffer

object VMBufferClass extends VMClass with VMBuilder {
  val parent: VMClass = VMObjectClass
  val name: String = "Buffer"
  val extending: List[VMType] = List(parent)
  val members: Map[Symbol, VMMember] = Map()
  val clas: VMClass = VMClassClass

  override def build(iterator: Iterator[VMObject]): VMObject = new VMBuffer(iterator to ArrayBuffer)
}

class VMBuffer private[bvm] (buf: ArrayBuffer[VMObject]) extends VMResizableSequence with VMNonMap {
  def this() = this(new ArrayBuffer[VMObject])

  val clas: VMClass = VMArrayClass

  override val isSequence: Boolean = true

  def iterator: Iterator[VMObject] = buf.iterator

  def apply(idx: Int): VMObject = buf(idx)

  def size: Int = buf.length

  def addOne(elem: VMObject): Unit = buf += elem

  override def addAll(seq: VMObject): Unit = buf ++= seq.iterator

  def subtractOne(elem: VMObject): Unit = buf -= elem

  override def subtractAll(seq: VMObject): Unit = buf --= seq.iterator

  def append(elem: VMObject): VMObject = {
    val buf1 = buf.iterator to ArrayBuffer

    buf1 += elem
    new VMBuffer(buf1)
  }

  override def toString: String = buf.mkString("Buffer(", ", ", ")")
}
