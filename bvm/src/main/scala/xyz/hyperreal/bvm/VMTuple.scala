package xyz.hyperreal.bvm

import scala.collection.immutable.ArraySeq

trait TupleLike extends VMNonAppendableNonResizableNonMapSequence with VMUnordered { //todo: should be ordered
  override def iterator: Iterator[VMObject] =
    new Iterator[VMObject] {
      private var idx = 0

      def hasNext: Boolean = idx < length

      def next(): VMObject =
        if (hasNext) {
          val res = apply(idx)

          idx += 1
          res
        } else
          throw new NoSuchElementException
    }
}

class RecordConstructor(val typename: String, val name: String, val fields: List[Symbol])
    extends VMNonResizableUniqueNonIterableObject
    with VMUnordered {
  val arity: Int = fields.length
  val symbolMap = Map(fields zipWithIndex: _*)
  val stringMap = Map(fields map (_.name) zipWithIndex: _*)
  val clas: VMClass = null //todo: fix

  override def toString: String = s"RecordConstructor($typename, $name, $fields)"
}

class VMRecord(val name: String,
               elems: IndexedSeq[VMObject],
               val symbolMap: Map[Symbol, Int],
               val stringMap: Map[String, Int])
    extends VMObject
    with TupleLike {

  val clas: VMClass = null //todo: figure this out

  def apply(idx: Int): VMObject = elems(idx)

  def size: Int = elems.length

  override def toString: String = if (size == 0) name else s"$name(${elems mkString ", "})"
}

class VMTuple(elems: IndexedSeq[VMObject]) extends VMObject with TupleLike {
  def this(p: Product) = this(p.productIterator.to(ArraySeq).asInstanceOf[ArraySeq[VMObject]])

  val clas: VMClass = null //todo: figure this out

  def apply(idx: Int): VMObject = elems(idx)

  def size: Int = elems.length

  override def toString = s"(${elems mkString ", "})"
}
