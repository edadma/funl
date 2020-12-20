package xyz.hyperreal.bvm

import scala.collection.mutable

trait Assignable {

  def value: Any

  def value_=(v: Any): Unit

}

class VariableAssignable(var value: Any) extends Assignable

class MutableSeqAssignable(seq: mutable.Seq[Any], index: Int) extends Assignable {
  def value: Any = seq(index)

  def value_=(v: Any): Unit = {
    seq match {
      case buf: mutable.Buffer[Any] if buf.length <= index =>
        buf.appendAll(Iterator.fill(index - buf.length + 1)(null))
      case _ =>
    }

    seq(index) = v
  }
}

class MutableMapAssignable(map: mutable.Map[Any, Any], key: Any) extends Assignable {
  def value: Any = map.getOrElse(key, undefined)

  def value_=(v: Any): Unit = map(key) = v
}
