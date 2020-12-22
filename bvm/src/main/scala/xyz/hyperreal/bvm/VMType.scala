package xyz.hyperreal.bvm

import xyz.hyperreal.dal.{Type, TypedNumber}

import java.util.NoSuchElementException

trait VMType {
  val name: String
  val extending: List[VMType]
  val members: Map[Symbol, VMMember]
}

trait VMMember {
  //val clas: VMClass
  val name: String
}

trait VMNativeMethod extends VMMember {
  def method: PartialFunction[(VM, VMObject, Any), Any]
}

trait VMClass extends VMType {
  val parent: VMClass
}

trait VMObject {
  val clas: VMClass

  def toString: String

  val isIterable: Boolean = false

  def iterator: Iterator[VMObject] = sys.error("no iterator method")

  val isSequence: Boolean = false

  def apply(idx: Int): VMObject = sys.error("no apply method")

  def length: Int = sys.error("no length method")

}

//trait VMInstance extends VMObject {
//  val outer: Option[VMInstance]
//}

trait VMNumber extends VMObject with TypedNumber

trait VMIterable extends VMObject {
  override val isIterable: Boolean = true
}

trait VMSequence extends VMIterable {
  override val isSequence: Boolean = true
}

trait VMList extends VMObject with VMSequence

trait VMCons extends VMList /*with VMInstance*/ { consThis =>
  val head: VMObject
  val tail: VMList

  override val isSequence: Boolean = true

  override def iterator: Iterator[VMObject] =
    new Iterator[VMObject] {
      var cur: VMList = consThis

      def hasNext: Boolean = cur.isInstanceOf[VMCons]

      def next(): VMObject = {
        cur match {
          case c: VMCons =>
            val res = c.head

            cur = c.tail
            res
          case _ => throw new NoSuchElementException("iterable has no more elements")
        }
      }
    }

  override def apply(idx: Int): VMObject = iterator.drop(idx).next()

  override def length: Int = iterator.length
}

trait VMNil extends VMList { //with VMInstance
  override def iterator: Iterator[VMObject] =
    new Iterator[VMObject] {
      def hasNext: Boolean = false

      def next(): VMObject = throw new NoSuchElementException("nil has no elements")
    }

  override def apply(idx: Int): VMObject = throw new IndexOutOfBoundsException

  override def length: Int = 0
}

object VMObjectClass extends VMClass {
  val parent: VMClass = null
  val name: String = "Object"
  val extending: List[VMType] = Nil
  val members: Map[Symbol, VMMember] = Map()
}

object VMListClass extends VMClass {
  val parent: VMClass = VMObjectClass
  val name: String = "List"
  val extending: List[VMType] = Nil
  val members: Map[Symbol, VMMember] = Map()
}

object VMConstClass extends VMClass {
  val parent: VMClass = VMListClass
  val name: String = "Const"
  val extending: List[VMType] = List(VMListClass)
  val members: Map[Symbol, VMMember] = Map()
}

class VMConsObject(val head: VMObject, val tail: VMList) extends VMCons {
//  val outer: Option[VMInstance] = None
  val clas: VMClass = VMConstClass
}

object VMNilObject extends VMNil {
  val clas: VMClass = VMListClass
//  val outer: Option[VMInstance] = None
}

object VMNumberClass extends VMClass {
  val parent: VMClass = VMObjectClass
  val name: String = "Number"
  val extending: List[VMType] = Nil
  val members: Map[Symbol, VMMember] = Map()
}

class VMNumberObject(val typ: Type, val value: Number) extends VMNumber {
  val clas: VMClass = VMNumberClass
}
