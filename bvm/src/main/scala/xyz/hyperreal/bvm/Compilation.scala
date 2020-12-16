package xyz.hyperreal.bvm

import scala.collection.immutable.ArraySeq


class Compilation( val functions: Map[String, (Int, Int)], val variables: Map[String, Int],
									 val constants: Map[String, Any], captureTrees: ArraySeq[Node],
									 val code: ArraySeq[VMInst] ) {

	def apply( idx: Int ) = code( idx )

	def length = code.length

	var matcherConstructor: (Boolean, Boolean, Any) => VM =
		(scan: Boolean, anchored: Boolean, args: Any) => new VM( this, captureTrees, scan, anchored, null )

	def matches( subject: CharSequence, scan: Boolean = true, anchored: Boolean = false ) = {
		val m = matcher( scan = scan, anchored = anchored )

		if (m.matches( subject ))
			Some( m.groups )
		else
			None
	}

	def allMatches( subject: CharSequence, scan: Boolean = true, anchored: Boolean = false ) = {
		val m = matcher( scan = scan, anchored = anchored )

		def allRematches: LazyList[Map[String, Any]] =
			if (m.rematches)
				m.groups #:: allRematches
			else
				LazyList.empty

		if (m.matches( subject ))
			m.groups #:: allRematches
		else
			LazyList.empty
	}

	def matcher( scan: Boolean = false, anchored: Boolean = false ) = matcherConstructor( scan, anchored, null )

}