package xyz.hyperreal.bvm

import xyz.hyperreal.dal.{Type, TypedNumber}

import java.util.NoSuchElementException
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

trait VMType {
  val name: String
  val extending: List[VMType]
  val members: Map[Symbol, VMMember]
}

abstract class VMMember {
  //val clas: VMClass
  val name: String
}

abstract class VMNativeMethod extends VMMember {
  def method: PartialFunction[(VM, VMObject, Any), Any]
}

abstract class VMClass extends VMObject with VMType {
  val name: String
  val parent: VMClass

  def canBuild: Boolean = false
  def build(from: Iterator[VMObject]): VMObject = sys.error("can't build from iterator")

  override def toString: String = s"class $name"
}

trait VMBuilder extends VMClass {
  override def canBuild: Boolean = true
}

abstract class VMObject {
  val clas: VMClass

  def toString: String

  val isIterable: Boolean = false

  def iterator: Iterator[VMObject] = sys.error("no iterator method")

  val isSequence: Boolean = false

  def apply(idx: Int): VMObject = sys.error("no apply method")

  def length: Int = sys.error("no length method")

}

trait VMInstance extends VMObject {
  val outer: Option[VMInstance]
}

object VMClassClass extends VMClass {
  val name: String = "Class"
  val parent: VMClass = VMObjectClass
  val extending: List[VMType] = List(parent)
  val members: Map[Symbol, VMMember] = Map()
  val clas: VMClass = VMClassClass
}

object VMNumberClass extends VMClass {
  val name: String = "Number"
  val parent: VMClass = VMObjectClass
  val extending: List[VMType] = List(parent)
  val members: Map[Symbol, VMMember] = Map()
  val clas: VMClass = VMClassClass
}

object VMNumber {
  def apply(n: (Type, Number)) = new VMNumber(n._1, n._2)
}

class VMNumber(val typ: Type, val value: Number) extends VMObject with TypedNumber {
  val clas: VMClass = VMNumberClass

  override def toString: String = value.toString
}

trait VMIterable extends VMObject {
  override val isIterable: Boolean = true
}

trait VMSequence extends VMIterable {
  override val isSequence: Boolean = true
}

trait VMList extends VMObject with VMSequence

object VMNil extends VMList with VMInstance {
  val clas: VMClass = VMListClass
  val outer: Option[VMInstance] = None

  override def iterator: Iterator[VMObject] =
    new Iterator[VMObject] {
      def hasNext: Boolean = false

      def next(): VMObject = throw new NoSuchElementException("nil has no elements")
    }

  override def apply(idx: Int): VMObject = throw new IndexOutOfBoundsException

  override def length: Int = 0

  override def toString: String = "[]"
}

object VMObjectClass extends VMClass {
  val parent: VMClass = null
  val name: String = "Object"
  val extending: List[VMType] = Nil
  val members: Map[Symbol, VMMember] = Map()
  val clas: VMClass = VMClassClass
}

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

class VMConsObject(val head: VMObject, var tail: VMList) extends VMList with VMInstance {
  val outer: Option[VMInstance] = None
  val clas: VMClass = VMConsClass

  override val isSequence: Boolean = true

  override def iterator: Iterator[VMObject] =
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

  override def apply(idx: Int): VMObject = iterator.drop(idx).next()

  override def length: Int = iterator.length

  override def toString: String = {
    val buf = new ListBuffer[VMObject]

    @tailrec
    def elem(l: VMList): Unit =
      l match {
        case VMNil =>
        case c: VMConsObject =>
          buf += c.head
          elem(c.tail)
      }

    elem(this)
    buf.mkString("[", ", ", "]")
  }
}
