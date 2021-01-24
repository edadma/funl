package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.char_reader.CharReader
import xyz.hyperreal.prolog.{PrologException, VM}

object Control {

  def halt(vm: VM, pos: IndexedSeq[CharReader]) = sys.exit()

  def halt(vm: VM, pos: IndexedSeq[CharReader], status: Any) = sys.exit(status.asInstanceOf[Int])

  def `throw`(vm: VM, pos: IndexedSeq[CharReader], error: Any) =
    error match {
      case _: vm.Variable => sys.error("error must be given")
      case e              => throw new PrologException("exception", e)
    }

}
