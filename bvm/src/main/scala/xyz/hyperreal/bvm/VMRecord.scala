package xyz.hyperreal.bvm

class RecordConstructor(val typename: String, val name: String, val fields: List[Symbol])
    extends VMNonResizableUniqueNonIterableObject
    with VMUnordered
    with VMNonUpdatable {
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
    with VMNonAppendableNonResizableNonMapSequence
    with VMUnordered
    with VMNonUpdatable
    with VMNonSet { //todo: should be ordered

  val clas: VMClass = null //todo

  def apply(idx: VMObject): VMObject = elems(idx.asInstanceOf[VMNumber].value.intValue)

  def size: Int = elems.length

  def iterator: Iterator[VMObject] = elems.iterator

  override def toString: String = if (size == 0) name else s"$name(${elems mkString ", "})"
}
