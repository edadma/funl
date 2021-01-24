package xyz.hyperreal.recursive_descent_parser

import xyz.hyperreal.char_reader.CharReader

abstract class AST {

  val pos: CharReader

}

case class IntegerAST(pos: CharReader, n: Int) extends AST

case class StringAST(pos: CharReader, s: String) extends AST

case class AtomAST(pos: CharReader, atom: String) extends AST

case class VariableAST(pos: CharReader, name: String) extends AST

case class AnonymousAST(pos: CharReader) extends AST

case class StructureAST(pos: CharReader, name: String, args: List[AST]) extends AST

case class ListAST(pos: CharReader, args: List[AST], last: Option[AST]) extends AST
