package xyz.hyperreal.bvm

import scala.annotation.tailrec

object VMListClass extends VMClass with VMBuilder {
  val parent: VMClass = VMObjectClass
  val name: String = "List"
  val extending: List[VMType] = List(parent)
  val members: Map[Symbol, VMMember] = Map()

  override def build(from: Iterator[VMObject]): VMObject = {
    var list: VMList = VMNil
    var last: VMConsObject = null

    while (from.hasNext) {
      if (last eq null) {
        last = new VMConsObject(from.next(), null)
        list = last
      } else
        last.tail = new VMConsObject(from.next(), last)
    }

    if (last ne null)
      last.tail = VMNil

    list
  }

  val clas: VMClass = VMClassClass
}

object VMConsClass extends VMClass {
  val parent: VMClass = VMListClass
  val name: String = "Cons"
  val extending: List[VMType] = List(VMListClass)
  val members: Map[Symbol, VMMember] = Map()
  val clas: VMClass = VMClassClass
}

class VMConsObject(val head: VMObject, var tail: VMList) extends VMList {
  val clas: VMClass = VMConsClass

  override val isSequence: Boolean = true

  def iterator: Iterator[VMObject] =
    new Iterator[VMObject] {
      var cur: VMList = VMConsObject.this

      def hasNext: Boolean = cur.isInstanceOf[VMConsObject]

      def next(): VMObject = {
        cur match {
          case c: VMConsObject =>
            val res = c.head

            cur = c.tail
            res
          case _ => throw new NoSuchElementException("iterable has no more elements")
        }
      }
    }

  def apply(idx: Int): VMObject = iterator.drop(idx).next()

  def length: Int = iterator.length

  override def toString: String = {
    val buf = new StringBuilder

    @tailrec
    def elem(l: VMList): Unit =
      l match {
        case VMNil =>
        case c: VMConsObject =>
          buf ++= c.head.toString

          if (c.tail != VMNil)
            buf ++= ", "

          elem(c.tail)
      }

    elem(this)
    s"[$buf]"
  }
}
