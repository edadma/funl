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

class VMBuffer(private[bvm] val buffer: ArrayBuffer[VMObject])
    extends VMResizableSequence
    with VMNonMap
    with VMUnordered
    with VMNonSet {
  def this() = this(new ArrayBuffer[VMObject])

  val clas: VMClass = VMArrayClass

  override val isSequence: Boolean = true

  def iterator: Iterator[VMObject] = buffer.iterator

  def apply(idx: VMObject): VMObject = buffer(idx.asInstanceOf[VMNumber].value.intValue)

  def size: Int = buffer.length

  def addOne(elem: VMObject): Unit = buffer += elem

  override def addAll(seq: VMObject): Unit = buffer ++= seq.iterator

  def subtractOne(elem: VMObject): Unit = buffer -= elem

  override def subtractAll(seq: VMObject): Unit = buffer --= seq.iterator

  def append(elem: VMObject): VMObject = new VMBuffer(buffer :+ elem)

  def concat(iterable: VMObject): VMObject = VMBufferClass.build(iterator ++ iterable.iterator)

  val isUpdatable: Boolean = true

  def update(key: VMObject, value: VMObject): Unit =
    key match {
      case VMNumber(IntType, n) => buffer(n.intValue) = value
    }

  override def toString: String = buffer.mkString("Buffer(", ", ", ")")

  def head: VMObject = buffer.head

  def tail: VMObject = new VMBuffer(buffer.tail)
}
