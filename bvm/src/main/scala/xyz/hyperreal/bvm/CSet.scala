package xyz.hyperreal.bvm

import collection.mutable.{ArrayBuffer, HashSet}


class CSet( classes: Any* ) extends (Char => Boolean) {

	private val union = new HashSet[Char]()
	private val buf = new ArrayBuffer[Char => Boolean]

	classes foreach[Unit] {
		case s: String => union ++= s
		case r: collection.immutable.NumericRange[_] => buf += (r contains _)
		case s: Iterable[_] => union ++= s.asInstanceOf[Iterable[Char]]
		case c: (Function[_, _]) => buf += c.asInstanceOf[Char => Boolean]
		case c: Char => union += c
	}

	if (union nonEmpty)
		buf += union

	def apply( c: Char ) = buf exists (_( c ))

	def complement = !this( _: Char )

}