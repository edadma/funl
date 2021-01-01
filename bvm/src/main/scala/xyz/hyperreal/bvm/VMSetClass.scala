package xyz.hyperreal.bvm

object VMSetClass extends VMClass with VMBuilder {
  val parent: VMClass = VMObjectClass
  val name: String = "Set"
  val extending: List[VMType] = List(parent)
  val members: Map[Symbol, VMMember] = Map()

  override def build(iterator: Iterator[VMObject]): VMObject = new VMSet(Set.from(iterator))

  val clas: VMClass = VMClassClass
}

class VMSet(set: Set[VMObject]) extends VMNonResizableIterable with VMNonSequence with VMNonMap with VMUnordered {
  val clas: VMClass = VMSetClass

  def iterator: Iterator[VMObject] = set.iterator

  def append(elem: VMObject): VMObject = new VMSet(set + elem)

  override def size: Int = set.size

  override def toString: String = iterator.map(displayQuoted).mkString("{", ", ", "}")
}

object VMEmptySet extends VMNonResizableIterable with VMNonSequence with VMNonMap with VMUnordered {
  val clas: VMClass = VMSetClass
//  val set: Set[VMObject] = Set.empty

  override def iterator: Iterator[VMObject] = Set.empty.iterator

  def append(elem: VMObject): VMObject = new VMSet(Set(elem))

  override def size: Int = 0

  override def toString: String = "{}"
}
