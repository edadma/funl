package xyz.hyperreal.bvm

object VMStringClass extends VMClass {
  val parent: VMClass = VMObjectClass
  val name: String = "String"
  val extending: List[VMType] = List(parent)
  val members: Map[Symbol, VMMember] = Map()
  val clas: VMClass = VMClassClass
}

case class VMString(string: String) extends VMImmutableSequence {
  val clas: VMClass = VMRangeClass

  def iterator: Iterator[VMObject] = string.iterator.map(c => new VMString(c.toString))

  def apply(idx: Int): VMObject = new VMString(string(idx).toString)

  def length: Int = string.length

  override def toString: String = string
}
