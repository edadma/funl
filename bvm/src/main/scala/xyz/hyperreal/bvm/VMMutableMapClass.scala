package xyz.hyperreal.bvm

import scala.collection.mutable

object VMMutableMapClass extends VMClass with VMBuilder {
  val parent: VMClass = VMObjectClass
  val name: String = "MutableMap"
  val extending: List[VMType] = List(parent)
  val members: Map[Symbol, VMMember] = Map()

  override def build(iterator: Iterator[VMObject]): VMObject =
    new VMMutableMap(mutable.Map.from(iterator.map(_.toPair)))

  val clas: VMClass = VMClassClass
}

class VMMutableMap private[bvm] (map: mutable.Map[VMObject, VMObject])
    extends VMObject
    with VMResizableIterableNonSequence
    with VMUnordered {
  def this() = this(mutable.Map.empty)
  val clas: VMClass = VMMutableMapClass
  val isMap = true

  def get(key: VMObject): Option[VMObject] = map get key

  def iterator: Iterator[VMObject] = map.iterator.map(new VMSeq(_))

  def size: Int = map.size

  def addOne(elem: VMObject): Unit =
    if (elem.isPair) map += elem.toPair
    else sys.error(s"not a tuple: $elem")

  def subtractOne(elem: VMObject): Unit = map -= elem

  override def hashCode: Int = map.hashCode

  override def equals(obj: Any): Boolean = map.equals(obj)

  val isUpdatable: Boolean = true

  def update(key: VMObject, value: VMObject): Unit = map(key) = value

  override def toString: String = {
    val keys =
      try {
        map.keysIterator.toList.sorted
      } catch {
        case e: Exception => map.keys
      }

    keys.map(k => s"${displayQuoted(k)}: ${displayQuoted(map(k))}").mkString("MutableMap(", ", ", ")")
  }
}
