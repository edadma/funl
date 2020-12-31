package xyz.hyperreal.bvm

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

abstract class VMClass extends VMNonResizableUniqueNonIterableObject with VMType {
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
  val outer: Option[VMObject] = None

  def isPair: Boolean = isInstanceOf[VMTuple] && size == 2

  def toPair: (VMObject, VMObject) = (apply(0), apply(1))

  val isIterable: Boolean

  def iterator: Iterator[VMObject]

  def append(elem: VMObject): VMObject

  def size: Int

  val isSequence: Boolean

  def apply(idx: Int): VMObject

  val isResizable: Boolean

  def addOne(elem: VMObject): Unit

  def addAll(seq: VMObject): Unit = seq.iterator foreach addOne

  def subtractOne(elem: VMObject): Unit

  def subtractAll(seq: VMObject): Unit = seq.iterator foreach subtractOne

  val isMap: Boolean

  def get(key: VMObject): Option[VMObject]

  def toString: String
}

trait VMNonIterable extends VMNonSequence with VMNonMap {
  val isIterable: Boolean = false

  def iterator: Iterator[VMObject] = sys.error("no iterator method")

  def size: Int = sys.error("no size method")

  def append(elem: VMObject): VMObject = sys.error("no append method")
}

trait VMResizableIterableNonSequence extends VMNonSequence with VMNonAppendable {
  val isResizable = true
  val isIterable = true
}

trait VMNonSequence {
  val isSequence: Boolean = false

  def apply(idx: Int): VMObject = sys.error("no apply method")
}

trait VMNonMap {
  val isMap: Boolean = false

  def get(key: VMObject): Option[VMObject] = sys.error("no get method")
}

abstract class VMNonSequenceObject extends VMObject with VMNonSequence with VMNonResizable

abstract class VMNonIterableObject extends VMObject with VMNonIterable with VMNonResizable

object VMClassClass extends VMClass {
  val name: String = "Class"
  val parent: VMClass = VMObjectClass
  val extending: List[VMType] = List(parent)
  val members: Map[Symbol, VMMember] = Map()
  val clas: VMClass = VMClassClass
}

trait VMNonResizable {
  val isResizable: Boolean = false

  def addOne(elem: VMObject): Unit = sys.error("can't add element")

  def subtractOne(elem: VMObject): Unit = sys.error("can't subtract element")
}

trait VMNonAppendable {
  def append(elem: VMObject): VMObject = sys.error("can't append")
}

trait VMNonResizableIterable extends VMObject with VMNonResizable {
  val isIterable: Boolean = true
}

trait VMResizableIterable extends VMObject {
  val isIterable: Boolean = true
  val isResizable: Boolean = true
}

trait VMNonResizableSequence extends VMNonResizableIterable {
  val isSequence: Boolean = true
}

trait VMNonAppendableNonResizableSequence extends VMNonResizableSequence with VMNonAppendable

trait VMNonAppendableNonResizableNonMapSequence extends VMNonAppendableNonResizableSequence with VMNonMap

trait VMResizableSequence extends VMResizableIterable {
  val isSequence: Boolean = true
}

object VMObjectClass extends VMClass {
  val parent: VMClass = null
  val name: String = "Object"
  val extending: List[VMType] = Nil
  val members: Map[Symbol, VMMember] = Map()
  val clas: VMClass = VMClassClass
}

object VMUnitClass extends VMClass {
  val parent: VMClass = VMObjectClass
  val name: String = "Unit"
  val extending: List[VMType] = List(VMObjectClass)
  val members: Map[Symbol, VMMember] = Map()
  val clas: VMClass = VMClassClass
}

object VMVoid extends VMNonResizableUniqueNonIterableObject {
  val clas: VMClass = VMUnitClass

  override def toString: String = "()"
}

abstract class VMNonResizableUniqueNonIterableObject extends VMObject with VMNonIterable with VMNonResizable

object VMUndefined extends VMNonResizableUniqueNonIterableObject {
  val clas: VMClass = null

  override def toString: String = "undefined"
}

object VMBooleanClass extends VMClass {
  val parent: VMClass = VMObjectClass
  val name: String = "Boolean"
  val extending: List[VMType] = List(VMObjectClass)
  val members: Map[Symbol, VMMember] = Map()
  val clas: VMClass = VMClassClass
}

trait VMBoolean {
  val b: Boolean

  override def hashCode(): Int = b.hashCode

  override def equals(obj: Any): Boolean = b == obj
}

object VMTrue extends VMNonResizableUniqueNonIterableObject with VMBoolean {
  val clas: VMClass = VMBooleanClass
  val b: Boolean = true

  override def toString: String = "true"
}

object VMFalse extends VMNonResizableUniqueNonIterableObject with VMBoolean {
  val clas: VMClass = VMBooleanClass
  val b: Boolean = false

  override def toString: String = "false"
}

object VMBoolean {
  def apply(b: Boolean): VMObject = if (b) VMTrue else VMFalse
}
