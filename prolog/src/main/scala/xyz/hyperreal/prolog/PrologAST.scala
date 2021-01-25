package xyz.hyperreal.prolog

import xyz.hyperreal.char_reader.CharReader

abstract class PrologAST

case class SourceAST(clauses: List[ClauseAST]) extends PrologAST

case class ClauseAST(term: TermAST) extends PrologAST

abstract class TermAST extends PrologAST {
  val pos: CharReader
}

case class StructureAST(pos: CharReader, name: String, args: List[TermAST]) extends TermAST

case class AtomAST(pos: CharReader, name: String) extends TermAST

case class StringAST(pos: CharReader, s: String) extends TermAST

case class VariableAST(pos: CharReader, var name: String) extends TermAST { var eval: Boolean = false }

case class AnonymousAST(pos: CharReader) extends TermAST

//case class ListAST( pos: CharReader, args: List[TermAST], last: Option[TermAST] ) extends TermAST

abstract class NumericAST extends TermAST {
  val v: Number
}

case class IntegerAST(pos: CharReader, v: Integer) extends NumericAST
case class FloatAST(pos: CharReader, v: java.lang.Double) extends NumericAST
