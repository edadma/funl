package xyz.hyperreal.bvm

import collection.mutable.ArrayBuffer
import scala.collection.mutable

class CSet(classes: Any*) extends (Char => Boolean) {

  private val union = new mutable.HashSet[Char]()
  private val buf = new ArrayBuffer[Char => Boolean]

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

}
