package xyz.hyperreal.bvm

import xyz.hyperreal.dal.TypedNumber

trait VMType {
  val name: String
  val extending: List[VMType]
  val members: Map[Symbol, VMMember]
}

trait VMMember {
  //val clas: VMClass
  val name: String
}

trait VMNativeMethod extends VMMember {
  def method: PartialFunction[(VM, VMObject, Any), Any]
}

trait VMClass extends VMType {
  val parent: VMClass
}

trait VMObject {
  val clas: VMClass

  def toString: String
}

trait VMInstance extends VMObject {
  val outer: Option[VMInstance]
}

trait VMNumber extends VMObject with TypedNumber

trait VMList extends VMObject

trait VMConst extends VMList with VMInstance {
  val head: VMObject
  val tail: VMObject
}

case object VMObjectClass extends VMClass {
  val parent: VMClass = null
  val name: String = "Object"
  val extending: List[VMType] = Nil
  val members: Map[Symbol, VMMember] = Map()
}

case object VMListClass extends VMClass {
  val parent: VMClass = VMObjectClass
  val name: String = "List"
  val extending: List[VMType] = Nil
  val members: Map[Symbol, VMMember] = Map()
}

case object VMConstClass extends VMClass {
  val parent: VMClass = VMListClass
  val name: String = "Const"
  val extending: List[VMType] = List(VMListClass)
  val members: Map[Symbol, VMMember] = Map()
}

case class VMConstObject(head: VMObject, tail: VMObject) extends VMConst {
  val outer: Option[VMInstance] = None
  val clas: VMClass = VMConstClass
}

case object VMNil extends VMList {
  val clas: VMClass = VMListClass
  val outer: Option[VMInstance] = None
}
