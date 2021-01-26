package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.bvm.VM
import xyz.hyperreal.char_reader.CharReader
import xyz.hyperreal.prolog.{CONS, Structure, domainError, instantiationError, list2array, typeError}

object Misc {

  def between(vm: VM, pos: IndexedSeq[CharReader], lower: Any, upper: Any, value: Any) =
    (lower, upper) match {
      case (_: vm.Variable, _) => instantiationError(pos(0), "lower must be given", "between", 3)
      case (_, _: vm.Variable) => instantiationError(pos(1), "upper must be given", "between", 3)
      case (l: Int, u: Int) if l <= u =>
        value match {
          case v: vm.Variable =>
            if (l == u)
              v bind l
            else {
              vm.resatisfyable(
                new (VM => Boolean) {
                  var next = l + 1

                  def apply(v1: VM): Boolean = {
                    if (next == u)
                      v bind u
                    else {
                      vm.resatisfyable(this)

                      val cur = next

                      next += 1
                      v bind cur
                    }
                  }
                }
              )
              v bind l
            }
          case x: Int => l <= x || x <= u
          case _      => typeError(pos(2), "value must be a variable or an integer", "integer", value, "between", 3)
        }
      case (l: Int, u: Int) => sys.error("lower must be less than or equal to upper")
    }

  def printf(vm: VM, pos: IndexedSeq[CharReader], format: Any, args: Any) =
    (format, args) match {
      case (f: String, a @ Structure(CONS, _)) =>
        list2array(a) match {
          case None        => sys.error(s"printf: not a proper list: $args")
          case Some(array) => Streams.output.print(f.format(array.toIndexedSeq: _*))
        }
      case _ => sys.error(s"printf: expected format string and list of arguments: $format, $args")
    }

}
