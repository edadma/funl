package xyz.hyperreal.bvm

import scala.collection.mutable

trait Assignable {
  def value: VMObject

  def value_=(v: VMObject): Unit
}

class VariableAssignable(var value: VMObject) extends VMNonResizableUniqueNonIterableObject with Assignable {
  val clas: VMClass = null

  override def toString: String = value.toString
}

class MutableSeqAssignable(seq: mutable.Seq[VMObject], index: Int) extends Assignable {
  def value: VMObject = seq(index)

  def value_=(v: VMObject): Unit = {
    seq match {
      case buf: mutable.Buffer[VMObject] if buf.length <= index =>
        buf.appendAll(Iterator.fill(index - buf.length + 1)(null))
      case _ =>
    }

    seq(index) = v
  }
}

class MutableMapAssignable(map: mutable.Map[VMObject, VMObject], key: VMObject) extends Assignable {
  def value: VMObject = map.getOrElse(key, VMUndefined)

  def value_=(v: VMObject): Unit = map(key) = v
}
