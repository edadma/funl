package xyz.hyperreal.bvm

object VMAtomClass extends VMClass {
  val parent: VMClass = VMObjectClass
  val name: String = "Atom"
  val extending: List[VMType] = List(parent)
  val members: Map[Symbol, VMMember] = Map()

  val clas: VMClass = VMClassClass
}

case class VMAtom(sym: Symbol) extends VMNonIterableObject with VMUnordered with VMNonUpdatable with VMNonSet {
  val clas: VMClass = VMCSetClass

  override def hashCode: Int = sym.hashCode

  override def equals(obj: Any): Boolean =
    obj match {
      case VMAtom(thatSym) => thatSym eq sym
      case _               => false
    }

  override def toString: String = s"Atom('${sym.name}')"
}
