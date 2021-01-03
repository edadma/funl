package xyz.hyperreal.bvm

import xyz.hyperreal.dal.IntType

import scala.collection.mutable.ArrayBuffer

object VMBufferClass extends VMClass with VMBuilder {
  val parent: VMClass = VMObjectClass
  val name: String = "Buffer"
  val extending: List[VMType] = List(parent)
  val members: Map[Symbol, VMMember] = Map()
  val clas: VMClass = VMClassClass

  override def build(iterator: Iterator[VMObject]): VMObject = new VMBuffer(iterator to ArrayBuffer)
}

class VMBuffer private[bvm] (buf: ArrayBuffer[VMObject])
    extends VMResizableSequence
    with VMNonMap
    with VMUnordered
    with VMNonSet {
  def this() = this(new ArrayBuffer[VMObject])

  val clas: VMClass = VMArrayClass

  override val isSequence: Boolean = true

  def iterator: Iterator[VMObject] = buf.iterator

  def apply(idx: VMObject): VMObject = buf(idx.asInstanceOf[VMNumber].value.intValue)

  def size: Int = buf.length

  def addOne(elem: VMObject): Unit = buf += elem

  override def addAll(seq: VMObject): Unit = buf ++= seq.iterator

  def subtractOne(elem: VMObject): Unit = buf -= elem

  override def subtractAll(seq: VMObject): Unit = buf --= seq.iterator

  def append(elem: VMObject): VMObject = new VMBuffer(buf :+ elem)

  def concat(iterable: VMObject): VMObject = VMBufferClass.build(iterator ++ iterable.iterator)

  val isUpdatable: Boolean = true

  def update(key: VMObject, value: VMObject): Unit =
    key match {
      case VMNumber(IntType, n) => buf(n.intValue) = value
    }

  override def toString: String = buf.mkString("Buffer(", ", ", ")")

  def head: VMObject = buf.head

  def tail: VMObject = new VMBuffer(buf.tail)
}
