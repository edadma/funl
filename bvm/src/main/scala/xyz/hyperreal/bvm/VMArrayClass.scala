package xyz.hyperreal.bvm

import scala.collection.mutable.ArrayBuffer

object VMArrayClass extends VMClass with VMBuilder {
  val parent: VMClass = VMObjectClass
  val name: String = "Array"
  val extending: List[VMType] = List(parent)
  val members: Map[Symbol, VMMember] = Map()
  val clas: VMClass = VMClassClass

  override def build(from: Iterator[VMObject]): VMObject = new VMArray(from to ArrayBuffer)
}

class VMArray private[bvm] (buf: ArrayBuffer[VMObject]) extends VMMutable {
  def this() = this(new ArrayBuffer[VMObject])

  val clas: VMClass = VMArrayClass

  override val isSequence: Boolean = true

  def iterator: Iterator[VMObject] = buf.iterator

  def apply(idx: Int): VMObject = buf(idx)

  def length: Int = buf.length

  def append(elem: VMObject): Unit = buf += elem

  def appendSeq(seq: VMObject): Unit = buf ++= seq.iterator

  def remove(elem: VMObject): Unit = buf -= elem

  def removeSeq(seq: VMObject): Unit = buf --= seq.iterator

  override def toString: String = buf.mkString("array(", ", ", ")")
}
