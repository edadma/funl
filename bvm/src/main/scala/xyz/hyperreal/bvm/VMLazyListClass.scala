package xyz.hyperreal.bvm

import scala.collection.immutable

object VMLazyListClass extends VMClass with VMBuilder {
  val parent: VMClass = VMObjectClass
  val name: String = "LazyList"
  val extending: List[VMType] = List(parent)
  val members: Map[Symbol, VMMember] = Map()

  override def build(iterator: Iterator[VMObject]): VMObject =
    new VMSeq(immutable.ArraySeq.from(iterator))

  val clas: VMClass = VMClassClass
}

class VMLazyList(list: LazyList[VMObject])
    extends VMObject
    with VMNonResizableSequence
    with VMNonMap
    with VMNonUpdatable
    with VMUnordered
    with VMNonSet {
  val clas: VMClass = VMSeqClass

  def apply(idx: VMObject): VMObject = list(idx.asInstanceOf[VMNumber].value.intValue)

  def size: Int = list.length

  def iterator: Iterator[VMObject] = list.iterator

  def append(elem: VMObject): VMObject = new VMLazyList(list :+ elem)

  def concat(iterable: VMObject): VMObject = new VMLazyList(list :++ iterable.iterator)

  def head: VMObject = list.head

  def tail: VMObject = new VMLazyList(list.tail)

  override def toString = s"LazyList(${list take 100 mkString ", "}${if (list isDefinedAt 101) ", ..." else ""})"

}
