package xyz.hyperreal.funl

import xyz.hyperreal.bvm.{VMClass, VMMember, VMObject, VMType}

abstract class FunlClass extends VMClass

object ObjectClass extends FunlClass {
  val name: String = "Object"
  val parent: VMClass = null
  val extending: List[VMType] = Nil
  val members: Map[Symbol, VMMember] = Map()
}

abstract class FunlObject extends VMObject
