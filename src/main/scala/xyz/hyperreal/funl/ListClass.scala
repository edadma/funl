package xyz.hyperreal.funl

import xyz.hyperreal.bvm.{VMClass, VMConst, VMInstance, VMMember, VMType}

object ListClass extends FunlClass {
  val name: String = "List"
  val parent: VMClass = ObjectClass
  val extending: List[VMType] = List(parent)
  val members: Map[Symbol, VMMember] = Map()
}

object ConsClass extends FunlClass {
  val name: String = "Cons"
  val parent: VMClass = ListClass
  val extending: List[VMType] = List(parent)
  val members: Map[Symbol, VMMember] = Map()
}

object NilObject extends FunlObject {
  val clas: VMClass = ListClass
}

class ConsObject(val head: FunlObject, val tail: FunlObject) extends FunlInstance with VMConst {
  val clas: VMClass = ConsClass
  val outer: Option[VMInstance] = None
}
