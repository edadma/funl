package xyz.hyperreal.recursive_descent_parser

import xyz.hyperreal.char_reader.CharReader

import scala.collection.mutable

abstract class Assoc(val name: String)
case object XFX extends Assoc("xfx")
case object YFX extends Assoc("yfx")
case object XFY extends Assoc("xfy")
case object FX extends Assoc("fx")
case object FY extends Assoc("fy")
case object XF extends Assoc("xf")
case object YF extends Assoc("yf")

case class Op(priority: Int, specifier: Assoc, operator: String)

object Builder {

  def apply[R](primary: Parser[R],
               ops: List[Op],
               unaryAction: (CharReader, String, R) => R,
               binaryAction: (R, CharReader, String, R) => R): (Map[Int, Parser[R]], List[String]) = {
    var higher = primary
    val ruleMap = new mutable.HashMap[Int, Parser[R]]

    def rule(cls: Assoc, same: Parser[R], fallback: Boolean, os: Set[String]) =
      cls match {
        case XFX => NonAssocInfix(higher, fallback, os, binaryAction)
        case YFX => LeftAssocInfix(higher, same, os, binaryAction)
        case XFY => RightAssocInfix(higher, same, os, binaryAction)
        case FX  => NonAssocPrefix(higher, fallback, os, unaryAction)
        case FY  => AssocPrefix(higher, same, os, unaryAction)
      }

    (ops groupBy (_.priority) toList) sortBy (_._1) map { case (p, os) => (p, os groupBy (_.specifier) toList) } foreach {
      case (p, List((cls, os))) =>
        higher = rule(cls, null, fallback = true, os map (_.operator) toSet)
        ruleMap(p) = higher
      case (p, cs) =>
        val same = new ParserRef[R]
        val alt = Alternates((cs map { case (cls, os) => rule(cls, same, fallback = false, os map (_.operator) toSet) }) :+ higher)

        same.ref = alt
        higher = alt
        ruleMap(p) = higher
    }

    (ruleMap toMap, ops.map(_.operator) ++ List("(", ")"))
  }

}
