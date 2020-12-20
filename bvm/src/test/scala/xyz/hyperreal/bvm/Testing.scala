package xyz.hyperreal.bvm

import java.io.ByteArrayOutputStream
import scala.collection.immutable.ArraySeq

object Testing {

  def run(ast: AST,
          constants: Map[String, Any],
          sysvars: Map[String, VM => Any],
          macros: Map[String, List[AST] => AST],
          args: Any*): Any = {
    val code = new Compiler(constants, sysvars, macros, comments = true).compile(ast)
    val vm = new VM(code, ArraySeq(), false, false, args, VMConstObject, VMNil)

    vm.execute
  }

  def runCapture(ast: AST,
                 constants: Map[String, Any],
                 sysvars: Map[String, VM => Any],
                 macros: Map[String, List[AST] => AST],
                 args: Any*): String = {
    val outCapture = new ByteArrayOutputStream

    Console.withOut(outCapture) { run(ast, constants, sysvars, macros) }
    outCapture.toString.trim
  }

}
