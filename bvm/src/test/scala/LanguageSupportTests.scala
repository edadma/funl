//@
package xyz.hyperreal.bvm

import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.util.parsing.input.Position


class LanguageSupportTests extends FreeSpec with ScalaCheckPropertyChecks with Matchers {

  val constants =
    Map(
      "write" -> {
        (_: VM, apos: Position, ps: List[Position], args: Any) =>
          val list =
            args match {
              case a: ArgList => a.array toList
              case a => List( a )
            }

          println( list map (a => display(deref(a))) mkString ", " )
      }
    )

 "hello world" in {
    val program =
      SourceAST( List(
        ValAST(
          VariableStructureAST( null, "a", "a" ),
          null,
          LiteralExpressionAST( "Hello world!" )
        ),
        ValAST(
          VariableStructureAST( null, "b", "b" ),
          null,
          LiteralExpressionAST( "bye bye" )
        ),
        ApplyExpressionAST(
          null,
          VariableExpressionAST( null, "write", "write" ),
          null,
          List(
            (null, VariableExpressionAST( null, "a", "a" ))
          ),
          false
        ),
        SetValueExpressionAST(
          null,
          "a",
          "a",
          VariableExpressionAST( null, "b", "b" )
        ),
        ApplyExpressionAST(
          null,
          VariableExpressionAST( null, "write", "write" ),
          null,
          List(
            (null, VariableExpressionAST( null, "a", "a" ))
          ),
          false
        )
        //, AssignmentExpressionAST( List((null, VariableExpressionAST(null, "a", "a"))), '=, null, List((null, LiteralExpressionAST( "bye bye again" ))) )
      ))

    runCapture( program, constants, Map(), Map() ) shouldBe
      """
        |Hello world!
        |bye bye
      """.stripMargin.trim
  }

}