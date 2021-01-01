package xyz.hyperreal.bvm

object VMArrayClass extends VMClass with VMBuilder {
  val parent: VMClass = VMObjectClass
  val name: String = "Array"
  val extending: List[VMType] = List(parent)
  val members: Map[Symbol, VMMember] = Map()
  val clas: VMClass = VMClassClass

  override def build(iterator: Iterator[VMObject]): VMObject = new VMArray(iterator to Array)
}

class VMArray private[bvm] (array: Array[VMObject]) extends VMNonResizableSequence with VMNonMap with VMUnordered { //todo: should be ordered
  def this(size: Int) = this(Array.fill[VMObject](size)(VMUndefined))

  val clas: VMClass = VMArrayClass

  override val isSequence: Boolean = true

  def iterator: Iterator[VMObject] = array.iterator

  def apply(idx: Int): VMObject = array(idx)

  def size: Int = array.length

  def append(elem: VMObject): VMObject = {
    val array1 = Array.copyAs[VMObject](array, array.length + 1)

    array1(array.length) = elem
    new VMArray(array1)
  }

  override def toString: String = array.mkString("Buffer(", ", ", ")")
}
