package xyz.hyperreal.bvm

import scala.collection.mutable

object VMMutableSetClass extends VMClass with VMBuilder {
  val parent: VMClass = VMObjectClass
  val name: String = "MutableSet"
  val extending: List[VMType] = List(parent)
  val members: Map[Symbol, VMMember] = Map()

  override def build(iterator: Iterator[VMObject]): VMObject = new VMMutableSet(mutable.Set.from(iterator))

  val clas: VMClass = VMClassClass
}

class VMMutableSet private[bvm] (set: mutable.Set[VMObject])
    extends VMNonResizableIterable
    with VMNonSequence
    with VMNonMap
    with VMUnordered {
  def this() = this(mutable.Set.empty)

  val clas: VMClass = VMMutableSetClass

  def iterator: Iterator[VMObject] = set.iterator

  //todo: addOne
  def append(elem: VMObject): VMObject = new VMMutableSet(set union mutable.Set(elem))

  override def size: Int = set.size

  override def toString: String = iterator.map(displayQuoted).mkString("MutableSet(", ", ", ")")
}

object VMEmptyMutableSet extends VMNonResizableIterable with VMNonSequence with VMNonMap with VMUnordered {
  val clas: VMClass = VMSetClass

  override def iterator: Iterator[VMObject] = mutable.Set.empty.iterator

  def append(elem: VMObject): VMObject = new VMMutableSet(mutable.Set(elem))

  override def size: Int = 0

  override def toString: String = "MutableSet()"
}
