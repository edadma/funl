package xyz.hyperreal.bvm

import scala.collection.mutable

trait Assignable {
  def value: VMObject

  def value_=(v: VMObject): Unit
}

class VariableAssignable(var value: VMObject)
    extends VMNonResizableUniqueNonIterableObject
    with Assignable
    with VMUnordered
    with VMNonUpdatable {
  val clas: VMClass = null

  override def toString: String = value.toString
}

class MutableSeqAssignable(seq: VMObject, index: Int) extends Assignable {
  def value: VMObject = seq(index)

  def value_=(v: VMObject): Unit = seq.update(VMNumber(index), v)
}

class MutableMapAssignable(map: VMObject, key: VMObject) extends Assignable {
  def value: VMObject = map.get(key).getOrElse(VMUndefined)

  def value_=(v: VMObject): Unit = map.update(key, v)
}
