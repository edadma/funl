package xyz.hyperreal.funl

import xyz.hyperreal.bvm.{VMClass, VMConst, VMInstance, VMList, VMMember, VMType}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object ListClass extends FunlClass {
  val name: String = "List"
  val parent: VMClass = ObjectClass
  val extending: List[VMType] = List(parent)
  val members: Map[Symbol, VMMember] = Map()
}

object ConsClass extends FunlClass {
  val name: String = "Cons"
  val parent: VMClass = ListClass
  val extending: List[VMType] = List(parent)
  val members: Map[Symbol, VMMember] = Map()
}

trait FunlList extends VMList

object NilObject extends FunlObject with FunlList {
  val clas: VMClass = ListClass
  val outer: Option[VMInstance] = None

  override def toString: String = "[]"
}

class ConsObject(val head: FunlObject, val tail: FunlList) extends FunlInstance with FunlList with VMConst {
  val clas: VMClass = ConsClass
  val outer: Option[VMInstance] = None

  override def toString: String = {
    val buf = new ListBuffer[FunlObject]

    @tailrec
    def elem(l: FunlList): Unit =
      l match {
        case NilObject =>
        case c: ConsObject =>
          buf += c.head
          elem(c.tail)
      }

    elem(this)
    buf.mkString("[", ", ", "]")
  }
}
