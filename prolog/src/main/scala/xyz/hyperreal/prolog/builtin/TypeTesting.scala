package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.bvm.{VM, VMAtom, VMNumber}
import xyz.hyperreal.char_reader.CharReader
import xyz.hyperreal.dal.{BigDecType, BigIntType, DoubleType, IntType}
import xyz.hyperreal.prolog.{DataStream, Structure, vareval}

object TypeTesting {

  def atom(vm: VM, pos: IndexedSeq[CharReader], a: Any): Boolean = a.isInstanceOf[VMAtom]

  def integer(vm: VM, pos: IndexedSeq[CharReader], a: Any): Boolean =
    a match {
      case VMNumber(IntType | BigIntType, _) => true
      case _                                 => false
    }

  def float(vm: VM, pos: IndexedSeq[CharReader], a: Any): Boolean =
    a match {
      case VMNumber(DoubleType | BigDecType, _) => true
      case _                                    => false
    }

  def number(vm: VM, pos: IndexedSeq[CharReader], a: Any): Boolean = a.isInstanceOf[VMNumber]

  def compound(vm: VM, pos: IndexedSeq[CharReader], a: Any): Boolean = a.isInstanceOf[Structure]

  def atomic(vm: VM, pos: IndexedSeq[CharReader], a: Any): Boolean = !a.isInstanceOf[VM#Variable] && !compound(vm, pos, a)

  def callable(vm: VM, pos: IndexedSeq[CharReader], a: Any): Boolean = atom(vm, pos, a) || compound(vm, pos, a)

  def ground(vm: VM, pos: IndexedSeq[CharReader], a: Any): Boolean =
    atomic(vm, pos, a) ||
      (vareval(a) match {
        case Structure(_, args) => args forall (ground(vm, pos, _))
        case _                  => false
      })

  def string(vm: VM, pos: IndexedSeq[CharReader], a: Any): Boolean = a.isInstanceOf[String]

  def is_stream(vm: VM, pos: IndexedSeq[CharReader], a: Any): Boolean = a.isInstanceOf[DataStream]

}
