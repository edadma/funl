package xyz.hyperreal.funl

import xyz.hyperreal.bvm.{VMClass, VMMember, VMType}

object ListClass extends FunlClass {
  val name: String = "List"
  val parent: VMClass = ObjectClass
  val extending: List[VMType] = List(parent)
  val members: Map[Symbol, VMMember] = Map()
}
