package xyz.hyperreal.bvm

import xyz.hyperreal.dal.{Type, TypedNumber, numberType}

object VMNumberClass extends VMClass {
  val name: String = "Number"
  val parent: VMClass = VMObjectClass
  val extending: List[VMType] = List(parent)
  val members: Map[Symbol, VMMember] = Map()
  val clas: VMClass = VMClassClass
}

object VMNumber {
  def apply(n: (Type, Number)) = new VMNumber(n._1, n._2)

  def apply(n: Number) = new VMNumber(numberType(n), n)
}

case class VMNumber(typ: Type, value: Number) extends VMNonIterableObject with TypedNumber {
  val clas: VMClass = VMNumberClass

  override def hashCode: Int = value.hashCode

  override def equals(obj: Any): Boolean = obj match {
    case n: VMNumber => value == n.value
    case _           => false
  }

  override def toString: String = value.toString
}
