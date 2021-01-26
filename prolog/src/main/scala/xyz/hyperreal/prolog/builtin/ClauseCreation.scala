package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.bvm.VM
import xyz.hyperreal.char_reader.CharReader

object ClauseCreation {

  def asserta(vm: VM, pos: IndexedSeq[CharReader], clause: Any) =
    clause match {
      case _: vm.Variable => sys.error("asserta: clause must be given")
      case _              =>
    }

}
