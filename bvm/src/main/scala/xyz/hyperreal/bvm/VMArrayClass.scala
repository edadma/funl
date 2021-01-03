package xyz.hyperreal.bvm

import xyz.hyperreal.dal.IntType

object VMArrayClass extends VMClass with VMBuilder {
  val parent: VMClass = VMObjectClass
  val name: String = "Array"
  val extending: List[VMType] = List(parent)
  val members: Map[Symbol, VMMember] = Map()
  val clas: VMClass = VMClassClass

  override def build(iterator: Iterator[VMObject]): VMObject = new VMArray(iterator to Array)
}

class VMArray private[bvm] (array: Array[VMObject])
    extends VMNonResizableSequence
    with VMNonMap
    with VMUnordered
    with VMNonSet { //todo: should be ordered
  def this(size: Int) = this(Array.fill[VMObject](size)(VMUndefined))

  val clas: VMClass = VMArrayClass

  override val isSequence: Boolean = true

  def iterator: Iterator[VMObject] = array.iterator

  def apply(idx: VMObject): VMObject = array(idx.toInt)

  def size: Int = array.length

  def append(elem: VMObject): VMObject = new VMArray(array :+ elem)

  def concat(iterable: VMObject): VMObject = VMArrayClass.build(iterator ++ iterable.iterator)

  val isUpdatable: Boolean = true

  def update(key: VMObject, value: VMObject): Unit =
    key match {
      case VMNumber(IntType, n) => array(n.intValue) = value
    }

  def head: VMObject = array.head

  def tail: VMObject = new VMArray(array.tail)

  override def toString: String = array.mkString("Array(", ", ", ")")
}
