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

class VMNumber(val typ: Type, val value: Number) extends VMObjectNotSeq with TypedNumber {
  val clas: VMClass = VMNumberClass

  override def toString: String = value.toString
}
