package xyz.hyperreal.bvm

object VMStringClass extends VMClass {
  val parent: VMClass = VMObjectClass
  val name: String = "String"
  val extending: List[VMType] = List(parent)
  val members: Map[Symbol, VMMember] = Map()
  val clas: VMClass = VMClassClass
}

case class VMString(string: String) extends VMNonResizableSequence with VMNonMap { //todo: comparable
  val clas: VMClass = VMRangeClass

  def iterator: Iterator[VMObject] = string.iterator.map(c => VMString(c.toString))

  def apply(idx: Int): VMObject = VMString(string(idx).toString)

  def size: Int = string.length

  override def append(elem: VMObject): VMObject = VMString(string :++ elem.toString)

  override def toString: String = string
}
