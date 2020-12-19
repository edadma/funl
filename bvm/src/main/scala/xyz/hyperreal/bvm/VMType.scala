package xyz.hyperreal.bvm

import xyz.hyperreal.dal.AbstractDALNumber

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
}

trait VMInstance extends VMObject {
  val outer: Option[VMInstance]
}

trait VMNumber extends VMObject with AbstractDALNumber {}

trait VMConst {
  val head: VMObject
  val tail: VMConst
}
