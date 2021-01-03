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
    extends VMObject
    with VMNonSequence
    with VMNonMap
    with VMUnordered
    with VMNonUpdatable {
  def this() = this(mutable.Set.empty)

  val clas: VMClass = VMMutableSetClass

  def iterator: Iterator[VMObject] = set.iterator

  def append(elem: VMObject): VMObject = new VMMutableSet(set union mutable.Set(elem))

  def concat(iterable: VMObject): VMObject = new VMMutableSet(set ++ iterable.iterator)

  override def size: Int = set.size

  val isIterable: Boolean = true
  val isSet: Boolean = true
  val isResizable: Boolean = true

  def contains(key: VMObject): Boolean = set(key)

  def addOne(elem: VMObject): Unit = set += elem

  def subtractOne(elem: VMObject): Unit = set -= elem

  override def toString: String = iterator.map(displayQuoted).mkString("MutableSet(", ", ", ")")
}
