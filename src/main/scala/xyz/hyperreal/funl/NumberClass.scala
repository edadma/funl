package xyz.hyperreal.funl

import xyz.hyperreal.bvm.{VMClass, VMMember, VMNumber, VMType}
import xyz.hyperreal.dal.Type

object NumberClass extends FunlClass {
  val name: String = "Number"
  val parent: VMClass = ObjectClass
  val extending: List[VMType] = List(parent)
  val members: Map[Symbol, VMMember] = Map()
}

object FunlNumber {
  def apply(n: (Type, Number)) = new FunlNumber(n._1, n._2)
}

class FunlNumber(val typ: Type, val value: Number) extends FunlObject with VMNumber {
  val clas: VMClass = NumberClass

  override def toString: String = value.toString
}
