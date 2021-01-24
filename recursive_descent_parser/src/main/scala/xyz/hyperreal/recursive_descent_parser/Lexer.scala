package xyz.hyperreal.recursive_descent_parser

import xyz.hyperreal.pattern_matcher.Matchers

import xyz.hyperreal.char_reader.CharReader

abstract class Token {
  val pos: CharReader
  val value: String
}

case class DoubleQuotedToken(pos: CharReader, value: String) extends Token
case class IdentToken(pos: CharReader, value: String) extends Token
case class SingleQuotedToken(pos: CharReader, value: String) extends Token
case class SymbolToken(pos: CharReader, value: String) extends Token
case class IntegerToken(pos: CharReader, value: String) extends Token
case class ErrorToken(pos: CharReader, value: String) extends Token
case class EOIToken(pos: CharReader) extends Token { val value = null }

class Lexer(delims: List[String]) extends Matchers[CharReader] {

  delimiters ++= delims

  override val lineComment = '%'

  def token =
    pos <~ eoi ^^ EOIToken |
      pos ~ singleStringLit ^^ { case p ~ n => SingleQuotedToken(p, n) } |
      pos ~ doubleStringLit ^^ { case p ~ n => DoubleQuotedToken(p, n) } |
      pos ~ ident ^^ { case p ~ n           => IdentToken(p, n) } |
      pos ~ delimiter ^^ { case p ~ d       => SymbolToken(p, d) } |
      pos ~ t(digits) ^^ { case p ~ n       => IntegerToken(p, n) } |
      pos ~ char ^^ { case p ~ c            => ErrorToken(p, c.toString) }

  def tokenStream(r: CharReader): LazyList[Token] = {
    token(r) match {
      case m: Mismatch         => m.error
      case Match(result, next) => result #:: tokenStream(next)
    }
  }

}
