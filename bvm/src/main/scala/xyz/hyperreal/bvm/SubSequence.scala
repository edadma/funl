package xyz.hyperreal.bvm

class SubSequence(val s: CharSequence, val begin: Int, val end: Int) extends CharSequence {
  require(0 <= begin, "begin must be greater than or equal to zero")
  require(begin <= end, "begin must be less than end")
  require(end <= s.length, "end must be less than or equal to length")

  def this(s: CharSequence) = {
    this(s, 0, s.length)
  }

  def length = end - begin

  def subSequence(i: Int, i1: Int) = new SubSequence(s, i + begin, i1 + begin)

  def charAt(i: Int) = s.charAt(i + begin)

  override def equals(o: Any) =
    o match {
      case s: String      => toString == s
      case s: SubSequence => toString == s.toString
      case _              => false
    }

  override def hashCode = toString.hashCode

  override def toString =
    s match {
      case str: String => str.substring(begin, end)
      case _           => (0 until length) map charAt mkString
    }
}
