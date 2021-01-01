package xyz.hyperreal.bvm

import scala.collection.immutable
import scala.collection.immutable.ArraySeq

object VMSeqClass extends VMClass with VMBuilder {
  val parent: VMClass = VMObjectClass
  val name: String = "Seq"
  val extending: List[VMType] = List(parent)
  val members: Map[Symbol, VMMember] = Map()

  override def build(iterator: Iterator[VMObject]): VMObject =
    new VMSeq(immutable.ArraySeq.from(iterator))

  val clas: VMClass = VMClassClass
}

class VMSeq(elems: IndexedSeq[VMObject])
    extends VMObject
    with VMNonAppendableNonResizableNonMapSequence
    with VMUnordered
    with VMNonUpdatable
    with VMNonSet {
  def this(p: Product) = this(p.productIterator.to(ArraySeq).asInstanceOf[ArraySeq[VMObject]])

  val clas: VMClass = VMSeqClass

  def apply(idx: VMObject): VMObject = elems(idx.asInstanceOf[VMNumber].value.intValue)

  def size: Int = elems.length

  def iterator: Iterator[VMObject] = elems.iterator

  override def toString = s"(${elems mkString ", "})"
}
