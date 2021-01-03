package xyz.hyperreal

import java.io.ByteArrayOutputStream
import scala.util.parsing.input.Position
import xyz.hyperreal.bvm._
import xyz.hyperreal.dal.{TypedNumber, Type}

import scala.collection.immutable.ArraySeq

package object funl {

  def problem(pos: Position, error: String): Nothing =
    if (pos eq null)
      sys.error(error)
    else
      sys.error(s"${pos.line}: $error\n${pos.longString}")

  def run(program: String,
          constants: Map[String, Any] = Map(),
          sysvars: Map[String, VM => Any] = Map(),
          args: Any = null): Any = {
    val parser = new FunLParser
    val ast = parser.parseFromString(program, parser.source).asInstanceOf[AST]
    val code = new Compiler(Predef.constants ++ constants, Predef.sysvars ++ sysvars, Predef.macros, comments = true)
      .compile(ast)
    val vm = new VM(
      code,
      ArraySeq(),
      false,
      false,
      args
    )

//    vm.trace = true
//    vm.limit = 300
    vm.execute
  }

  def runCapture(program: String): String = {
    val outCapture = new ByteArrayOutputStream

    Console.withOut(outCapture) {
      run(program)
    }

    outCapture.toString.trim
  }

}
