package xyz.hyperreal.bvm

import scala.collection.mutable.{Buffer, Seq => MutableSeq, Map => MutableMap}

trait Assignable {

  def value: Any

  def value_=(v: Any): Unit

}

class VariableAssignable(var value: Any) extends Assignable

class MutableSeqAssignable(seq: MutableSeq[Any], index: Int)
    extends Assignable {
  def value = seq(index)

  def value_=(v: Any) = {
    seq match {
      case buf: Buffer[Any] if buf.length <= index =>
        buf.appendAll(Iterator.fill(index - buf.length + 1)(null))
      case _ =>
    }

    seq(index) = v
  }
}

class MutableMapAssignable(map: MutableMap[Any, Any], key: Any)
    extends Assignable {
  def value = map.getOrElse(key, undefined)

  def value_=(v: Any) = map(key) = v
}
