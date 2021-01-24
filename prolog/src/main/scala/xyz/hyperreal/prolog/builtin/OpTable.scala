package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.char_reader.CharReader
import xyz.hyperreal.prolog.{Operator, Structure, VM, domainError, typeError}

object OpTable {

  def op(vm: VM, pos: IndexedSeq[CharReader], priority: Any, specifier: Any, operator: Any) =
    (priority, specifier, operator) match {
      case (p: Int, s: Symbol, _) =>
        true
      case _ => sys.error("op/3: expected integer, atom, atom")
    }

}
