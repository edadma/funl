package xyz.hyperreal.bvm

trait TupleLike extends VMNonAppendableNonResizableSequence {
//  def arity: Int

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
    extends VMNonResizableUniqueNonIterableObject {
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
  //  val arity: Int = elems.length

  val clas: VMClass = null //todo: figure this out

  def apply(idx: Int): VMObject = elems(idx)

  def length: Int = elems.length

  override def toString: String = if (length == 0) name else s"$name(${elems mkString ", "})"
}

class Tuple(elems: IndexedSeq[VMObject]) extends VMObject with TupleLike {
//  val arity: Int = elems.length

  val clas: VMClass = null //todo: figure this out

  def apply(idx: Int): VMObject = elems(idx)

  def length: Int = elems.length

  override def toString = s"(${elems mkString ", "})"
}
