package xyz.hyperreal.bvm

object VMCSetClass extends VMClass with VMBuilder {
  val parent: VMClass = VMObjectClass
  val name: String = "CSet"
  val extending: List[VMType] = List(parent)
  val members: Map[Symbol, VMMember] = Map()

  override def build(iterator: Iterator[VMObject]): VMObject = new VMSet(Set.from(iterator))

  val clas: VMClass = VMClassClass
}

case class VMCSet(cset: CSet) extends VMNonIterableObject with VMUnordered with VMNonUpdatable {
  val clas: VMClass = VMCSetClass

  def contains(key: VMObject): Boolean = cset(key.toString.head)

  val isSet = true

  override def hashCode: Int = cset.hashCode

  override def equals(obj: Any): Boolean =
    obj match {
      case c: VMCSet => c.cset == cset
      case _         => false
    }

  override def toString: String = cset.toString
}
