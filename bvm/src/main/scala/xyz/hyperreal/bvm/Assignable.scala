package xyz.hyperreal.bvm

object VMAssignableClass extends VMClass {
  val parent: VMClass = VMObjectClass
  val name: String = "Assignable"
  val extending: List[VMType] = List(VMObjectClass)
  val members: Map[Symbol, VMMember] = Map()
  val clas: VMClass = VMClassClass
}

abstract class Assignable extends VMNonResizableUniqueNonIterableObject with VMUnordered with VMNonUpdatable {
  def value: VMObject

  def value_=(v: VMObject): Unit

  val clas: VMClass = VMAssignableClass

  override def toString: String = sys.error("problem")
}

class VariableAssignable(var value: VMObject) extends Assignable {
  override def toString: String = value.toString
}

class MutableSeqAssignable(seq: VMObject, index: Int) extends Assignable {
  def value: VMObject = seq(index)

  def value_=(v: VMObject): Unit = seq.update(VMNumber(index), v)
  VMAssignableClass
}

class MutableMapAssignable(map: VMObject, key: VMObject) extends Assignable {
  def value: VMObject = map.get(key).getOrElse(VMUndefined)

  def value_=(v: VMObject): Unit = map.update(key, v)
}
