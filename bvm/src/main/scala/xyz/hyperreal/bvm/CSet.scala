package xyz.hyperreal.bvm

import collection.mutable.ArrayBuffer
import scala.collection.mutable

object VMCSetClass extends VMClass with VMBuilder {
  val parent: VMClass = VMObjectClass
  val name: String = "CSet"
  val extending: List[VMType] = List(parent)
  val members: Map[Symbol, VMMember] = Map()

  override def build(iterator: Iterator[VMObject]): VMObject = new VMSet(Set.from(iterator))

  val clas: VMClass = VMClassClass
}

class CSet(classes: Any*) extends VMNonIterableObject with VMUnordered with VMNonUpdatable with (Char => Boolean) {

  val clas: VMClass = VMCSetClass

  private val union = new mutable.HashSet[Char]()
  private[bvm] val buf = new ArrayBuffer[Char => Boolean]

  classes foreach [Unit] {
    case VMString(s)                             => union ++= s
    case s: String                               => union ++= s
    case r: collection.immutable.NumericRange[_] => buf += (r contains _)
    case s: Iterable[_]                          => union ++= s.asInstanceOf[Iterable[Char]]
    case c: Function[_, _]                       => buf += c.asInstanceOf[Char => Boolean]
    case c: Char                                 => union += c
  }

  if (union nonEmpty)
    buf += union

  def apply(c: Char): Boolean = buf exists (_(c))

  def complement: Char => Boolean = !this(_: Char)

  override def hashCode: Int = buf.hashCode()
  override def equals(obj: Any): Boolean =
    obj match {
      case c: CSet => c.buf == buf
      case _       => false
    }

}
