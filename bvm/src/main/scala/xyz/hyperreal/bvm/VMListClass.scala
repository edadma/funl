package xyz.hyperreal.bvm

import java.util.NoSuchElementException
import scala.collection.mutable.ArrayBuffer

object VMListClass extends VMClass with VMBuilder {
  val parent: VMClass = VMObjectClass
  val name: String = "List"
  val extending: List[VMType] = List(parent)
  val members: Map[Symbol, VMMember] = Map()

  override def build(iterator: Iterator[VMObject]): VMObject = {
    var list: VMList = VMNil
    var last: VMConsObject = null

    while (iterator.hasNext) {
      val next = new VMConsObject(iterator.next(), VMNil)

      if (last eq null)
        list = next
      else
        last.tail = next

      last = next
    }

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

case class VMConsObject(head: VMObject, var tail: VMList) extends VMList {
  val clas: VMClass = VMConsClass

  def iterator: Iterator[VMObject] =
    new Iterator[VMObject] {
      var cur: VMList = VMConsObject.this

      def hasNext: Boolean = cur.isInstanceOf[VMConsObject]

      def next(): VMObject =
        cur match {
          case VMConsObject(head, tail) =>
            cur = tail
            head
          case _ => throw new NoSuchElementException("iterable has no more elements")
        }
    }

  def apply(idx: VMObject): VMObject = iterator.drop(idx.toInt).next()

  def size: Int = iterator.length

  override def append(elem: VMObject): VMObject = {
    val buf = new ArrayBuffer[VMObject]

    buf ++= iterator
    buf += elem
    VMListClass.build(buf.iterator)
  }

  override def toString: String = iterator.map(displayQuoted).mkString("[", ", ", "]")
}

trait VMList
    extends VMObject
    with VMNonResizableSequence
    with VMNonMap
    with VMUnordered
    with VMNonUpdatable
    with VMNonSet //todo: should be ordered

object VMNil extends VMList {
  val clas: VMClass = VMListClass

  override def iterator: Iterator[VMObject] =
    new Iterator[VMObject] {
      def hasNext: Boolean = false

      def next(): VMObject = throw new NoSuchElementException("nil has no elements")
    }

  override def apply(idx: VMObject): VMObject = throw new IndexOutOfBoundsException

  override def size: Int = 0

  override def append(elem: VMObject): VMObject = VMConsObject(elem, VMNil)

  def head: VMObject = sys.error("empty list")

  def tail: VMObject = sys.error("empty list")

  override def toString: String = "[]"
}
