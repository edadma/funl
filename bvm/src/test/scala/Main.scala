//@
package xyz.hyperreal.bvm

import scala.util.parsing.input.Position

object Main extends App {

  val constants =
    Map(
      "write" -> { (_: VM, apos: Position, ps: List[Position], args: Any) =>
        val list =
          args match {
            case a: ArgList => a.array toList
            case a          => List(a)
          }

        println(list map (a => display(deref(a))) mkString ", ")
      }
    )

  val regex = PatternParser.parseRegexWithResult("(a)(b)").getOrElse(sys.error("regex error"))

  println(regex)

  val program =
    SourceAST(
      List(
        ApplyExpressionAST(
          null,
          VariableExpressionAST(null, "write", "write"),
          null,
          List((null, ScanExpressionAST(null, LiteralExpressionAST("asdf"), PatternExpressionAST(regex)))),
          tailrecursive = false
        )
      ))

  run(program, constants, Map(), Map(), null)
}
