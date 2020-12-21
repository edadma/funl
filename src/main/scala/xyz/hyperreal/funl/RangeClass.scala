package xyz.hyperreal.funl

import xyz.hyperreal.bvm.{VMClass, VMIterable, VMMember, VMObject, VMType}

object RangeClass extends FunlClass {
  val parent: VMClass = ObjectClass
  val name: String = "Range"
  val extending: List[VMType] = List(parent)
  val members: Map[Symbol, VMMember] = Map()
}

class RangeObject extends FunlObject with VMIterable {
  val clas: VMClass = RangeClass

  override def iterator: Iterator[VMObject] =

}
