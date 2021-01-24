package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.char_reader.CharReader
import xyz.hyperreal.prolog.{Structure, VM, instantiationError, typeError}

import scala.collection.mutable

object GlobalVariables {

  val vars = new mutable.HashMap[Symbol, Any]

}
