package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.bvm.VM
import xyz.hyperreal.char_reader.CharReader
import xyz.hyperreal.prolog.{Operator, Structure, domainError, typeError}

object OpTable {

  def op(vm: VM, pos: IndexedSeq[CharReader], priority: Any, specifier: Any, operator: Any): Boolean =
    (priority, specifier, operator) match {
      case (p: Int, s: Symbol, _) =>
        true
      case _ => sys.error("op/3: expected integer, atom, atom")
    }

}
