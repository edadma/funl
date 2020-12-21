package xyz.hyperreal.bvm

import xyz.hyperreal.dal.TypedNumber

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

trait VMInstance extends VMObject {
  val outer: Option[VMInstance]
}

trait VMNumber extends VMObject with TypedNumber

trait VMList extends VMObject

trait VMCons extends VMList with VMInstance { consThis =>
  val head: VMObject
  val tail: VMList

  override val isIterable: Boolean = true
  override val isSequence: Boolean = true

  override def iterator: Iterator[VMObject] =
    new Iterator[VMObject] {
      var cur: VMList = consThis

      def hasNext: Boolean = !cur.isInstanceOf[VMNil]

      def next(): VMObject = {
        if (!hasNext) throw new NoSuchElementException("iterable has no more elements")

        val res = cur

        cur = cur.asInstanceOf[VMCons].tail
        res
      }
    }

  override def apply(idx: Int): VMObject = iterator.drop(idx).next()

  override def length: Int = iterator.length
}

trait VMNil extends VMList with VMInstance

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
  val outer: Option[VMInstance] = None
  val clas: VMClass = VMConstClass
}

object VMNilObject extends VMNil {
  val clas: VMClass = VMListClass
  val outer: Option[VMInstance] = None
}
