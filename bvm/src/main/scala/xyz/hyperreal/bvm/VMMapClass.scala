package xyz.hyperreal.bvm

object VMMapClass extends VMClass with VMBuilder {
  val parent: VMClass = VMObjectClass
  val name: String = "Map"
  val extending: List[VMType] = List(parent)
  val members: Map[Symbol, VMMember] = Map()

  override def build(iterator: Iterator[VMObject]): VMObject =
    new VMMap(Map.from(iterator.map(t => (t.apply(0), t.apply(1)))))

  val clas: VMClass = VMClassClass
}

class VMMap(map: Map[VMObject, VMObject]) extends VMNonResizableIterable with VMNonSequence {
  val clas: VMClass = VMMapClass

  val isMap = true

  def get(key: VMObject): Option[VMObject] = map get key

  def iterator: Iterator[VMObject] = map.iterator.map(new VMTuple(_))

  override def size: Int = map.size

  def append(elem: VMObject): VMObject =
    if (elem.isInstanceOf[VMTuple] && elem.size == 2)
      new VMMap(map + ((elem(0), elem(1))))
    else
      sys.error(s"map entry should be a tuple")

  override def toString: String = iterator.map(displayQuoted).mkString("{", ", ", "}")
}

object VMEmptyMap extends VMNonResizableIterable with VMNonSequence {
  val clas: VMClass = VMSetClass

  val isMap = true

  def get(key: VMObject): Option[VMObject] = None

  override def iterator: Iterator[VMObject] = Iterator()

  override def size: Int = 0

  def append(elem: VMObject): VMObject = new VMSet(Set(elem))

  override def toString: String = "{}"
}
