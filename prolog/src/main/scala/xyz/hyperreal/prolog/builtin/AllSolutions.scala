package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.char_reader.CharReader
import xyz.hyperreal.prolog.{Compilation, Program, VM, Vars, array2list}

import scala.collection.mutable.ArrayBuffer

object AllSolutions {

  def findall(vm: VM, pos: IndexedSeq[CharReader], template: Any, goal: Any, instances: Any) =
    if (TypeTesting.callable(vm, pos, goal)) {
      implicit val prog = new Program
      implicit val vars = new Vars
      val block = prog.block("findall")

      Compilation.compileGoal(goal, vm.prog)

      val results = new ArrayBuffer[Any]

      if (vm.runblock(block)) {
        results += 123

        while (vm.rerunblock(block)) {
          results += 123
        }
      }

      vm.success = true
      vm.unify(array2list(results), instances)
    } else
      sys.error(s"findall: goal must be callable")

}
