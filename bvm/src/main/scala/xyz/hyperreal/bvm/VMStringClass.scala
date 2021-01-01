package xyz.hyperreal.bvm

object VMStringClass extends VMClass {
  val parent: VMClass = VMObjectClass
  val name: String = "String"
  val extending: List[VMType] = List(parent)
  val members: Map[Symbol, VMMember] = Map()
  val clas: VMClass = VMClassClass
}

case class VMString(string: String) extends VMNonResizableSequence with VMNonMap with VMNonUpdatable with VMNonSet { //todo: comparable
  val clas: VMClass = VMRangeClass

  def iterator: Iterator[VMObject] = string.iterator.map(c => VMString(c.toString))

  def apply(idx: VMObject): VMObject = VMString(string(idx.asInstanceOf[VMNumber].value.intValue).toString)

  def size: Int = string.length

  override def append(elem: VMObject): VMObject = VMString(string :++ elem.toString)

  def isOrdered: Boolean = true

  def compare(that: VMObject): Int =
    that match {
      case VMString(s) => string compareTo s
      case _           => sys.error(s"String is not comparable to $that")
    }

  override def toString: String = string
}
