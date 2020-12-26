package xyz.hyperreal.bvm

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

abstract class VMClass extends VMImmutableUniqueNonIterableObject with VMType {
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

  val isIterable: Boolean

  def iterator: Iterator[VMObject]

  val isSequence: Boolean

  def apply(idx: Int): VMObject

  def length: Int

  val isMutable: Boolean

  def append(elem: VMObject): Unit

  def appendSeq(seq: VMObject): Unit

  def remove(elem: VMObject): Unit

  def removeSeq(seq: VMObject): Unit

  def toString: String
}

abstract class VMNonUniqueObject extends VMObject {
  def hashCode: Int

  def equals(obj: Any): Boolean
}

trait VMNonIterable extends VMNonSequence {
  val isIterable: Boolean = false

  def iterator: Iterator[VMObject] = sys.error("no iterator method")
}

trait VMNonSequence {
  val isSequence: Boolean = false

  def apply(idx: Int): VMObject = sys.error("no apply method")

  def length: Int = sys.error("no length method")
}

abstract class VMNonSequenceObject extends VMNonUniqueObject with VMNonSequence with VMImmutable

abstract class VMNonIterableObject extends VMNonUniqueObject with VMNonIterable with VMImmutable

object VMClassClass extends VMClass {
  val name: String = "Class"
  val parent: VMClass = VMObjectClass
  val extending: List[VMType] = List(parent)
  val members: Map[Symbol, VMMember] = Map()
  val clas: VMClass = VMClassClass
}

trait VMImmutable {
  val isMutable: Boolean = false

  def append(elem: VMObject): Unit = sys.error("can't append element method")

  def appendSeq(seq: VMObject): Unit = sys.error("can't append sequence method")

  def remove(elem: VMObject): Unit = sys.error("can't remove element method")

  def removeSeq(seq: VMObject): Unit = sys.error("can't remove sequence method")
}

trait VMImmutableIterable extends VMObject with VMImmutable {
  val isIterable: Boolean = true
}

trait VMMutableIterable extends VMObject {
  val isIterable: Boolean = true
  val isMutable: Boolean = true
}

trait VMImmutableSequence extends VMImmutableIterable {
  val isSequence: Boolean = true
}

trait VMMutableSequence extends VMMutableIterable {
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

object VMVoid extends VMImmutableUniqueNonIterableObject {
  val clas: VMClass = VMUnitClass

  override def toString: String = "()"
}

abstract class VMImmutableUniqueNonIterableObject extends VMObject with VMNonIterable with VMImmutable

object VMUndefined extends VMImmutableUniqueNonIterableObject {
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

object VMTrue extends VMImmutableUniqueNonIterableObject with VMBoolean {
  val clas: VMClass = VMBooleanClass
  val b: Boolean = true

  override def toString: String = "true"
}

object VMFalse extends VMImmutableUniqueNonIterableObject with VMBoolean {
  val clas: VMClass = VMBooleanClass
  val b: Boolean = false

  override def toString: String = "false"
}

object VMBoolean {
  def apply(b: Boolean): VMObject = if (b) VMTrue else VMFalse
}
