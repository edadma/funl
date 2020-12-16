package xyz.hyperreal.bvm

import scala.collection.mutable.{ArrayBuffer, HashSet}


object Pattern {

	val DOTALL = 0x01
	val UNIX_LINES = 0x02
	val CASE_INSENSITIVE = 0x04
	val COMMENTS = 0x08
	val MULTILINE = 0x10
	val UNICODE_CASE = 0x20
	val UNICODE_CHARACTER_CLASS = 0x40

	def memberOf( seq: Seq[Char] ) = (c: Char) => seq contains c

	def unionOf( classes: (Char => Boolean)* ) = {
		val union = new HashSet[Char]()
		val buf = new ArrayBuffer[Char => Boolean]

		classes foreach[Unit] {
			case s: Set[Char] => union ++= s
			case c => buf += c
		}

		buf += union
		(c: Char) => buf exists (_( c ))
	}

	def except( clas: Char => Boolean ) = !clas( _: Char )

	def compiledSubpattern( pat: PatternAST ) = CompiledSubPattern( new Compiler(Map(), Map(), Map()).compile(pat), new Compiler(Map(), Map(), Map(), Reverse).compile(pat) )

	def compile( regex: String ) = new Compiler( Map(), Map(), Map() ).compile( NumberedCapturePattern(PatternParser.parseRegex(regex)) )

	def compileReverse( regex: String ) = new Compiler( Map(), Map(), Map(), Reverse ).compile( NumberedCapturePattern(PatternParser.parseRegex(regex)) )

}
