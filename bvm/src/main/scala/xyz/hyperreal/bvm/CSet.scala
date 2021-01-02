package xyz.hyperreal.bvm

import collection.mutable.ArrayBuffer
import scala.collection.immutable.NumericRange
import scala.collection.mutable

class CSet(classes: List[Any]) extends (Char => Boolean) {

  def this(s: String) = this(List(s))
  def this(r: NumericRange[Char]) = this(List(r))
  def this(f: Char => Boolean) = this(List(f))

  private val union = new mutable.HashSet[Char]()
  private[bvm] val buf = new ArrayBuffer[Char => Boolean]

  classes foreach [Unit] {
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

  override def hashCode: Int = buf.hashCode

  override def equals(obj: Any): Boolean =
    obj match {
      case c: CSet => c.buf == buf
      case _       => false
    }

}
