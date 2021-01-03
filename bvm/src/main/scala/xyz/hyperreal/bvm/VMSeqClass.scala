package xyz.hyperreal.bvm

import scala.collection.immutable

object VMSeqClass extends VMClass with VMBuilder {
  val parent: VMClass = VMObjectClass
  val name: String = "Seq"
  val extending: List[VMType] = List(parent)
  val members: Map[Symbol, VMMember] = Map()

  override def build(iterator: Iterator[VMObject]): VMObject =
    new VMSeq(immutable.ArraySeq.from(iterator))

  val clas: VMClass = VMClassClass
}

class VMSeq(seq: IndexedSeq[VMObject])
    extends VMObject
    with VMNonResizableSequence
    with VMNonMap
    with VMNonUpdatable
    with VMUnordered
    with VMNonSet {
  def this(p: Product) = this(p.productIterator.to(immutable.ArraySeq).asInstanceOf[immutable.ArraySeq[VMObject]])

  val clas: VMClass = VMSeqClass

  def apply(idx: VMObject): VMObject = seq(idx.asInstanceOf[VMNumber].value.intValue)

  def size: Int = seq.length

  def iterator: Iterator[VMObject] = seq.iterator

  def append(elem: VMObject): VMObject = new VMSeq(seq :+ elem)

  def concat(iterable: VMObject): VMObject = new VMSeq(seq :++ iterable.iterator)

  def head: VMObject = seq.head

  def tail: VMObject = new VMSeq(seq.tail)

  override def toString = s"(${seq mkString ", "})"
}
