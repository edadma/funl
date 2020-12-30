package xyz.hyperreal.bvm

import scala.collection.mutable

object VMMutableMapClass extends VMClass with VMBuilder {
  val parent: VMClass = VMObjectClass
  val name: String = "MutableMap"
  val extending: List[VMType] = List(parent)
  val members: Map[Symbol, VMMember] = Map()

  override def build(iterator: Iterator[VMObject]): VMObject =
    new VMMutableMap(mutable.Map.from(iterator.map(t => (t(0), t(1)))))

  val clas: VMClass = VMClassClass
}

class VMMutableMap private[bvm] (map: mutable.Map[VMObject, VMObject])
    extends VMObject
    with VMResizableIterableNonSequence {
  val clas: VMClass = VMMutableMapClass

  val isMap = true

  def get(key: VMObject): Option[VMObject] = map get key

  def iterator: Iterator[VMObject] = map.iterator.map(new VMTuple(_))

  def size: Int = map.size

  def addOne(elem: VMObject): Unit = ???

  def addAll(seq: VMObject): Unit = ???

  def subtractOne(elem: VMObject): Unit = ???

  def subtractAll(seq: VMObject): Unit = ???

  override def toString: String =
    map.iterator.map { case (k, v) => s"${displayQuoted(k)}: ${displayQuoted(v)}" }.mkString("MutableMap(", ", ", ")")
}

object VMEmptyMutableMap extends VMObject with VMResizableIterableNonSequence {
  val clas: VMClass = VMMapClass

  val isMap = true

  def get(key: VMObject): Option[VMObject] = None

  def iterator: Iterator[VMObject] = Iterator()

  def size: Int = 0

//      sys.error(s"expected tuple")

  def addOne(elem: VMObject): Unit = ???

  def addAll(seq: VMObject): Unit = ???

  def subtractOne(elem: VMObject): Unit = ???

  def subtractAll(seq: VMObject): Unit = ???

  override def toString: String = "MutableMap()"
}
