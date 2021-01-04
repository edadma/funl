package xyz.hyperreal.bvm

import xyz.hyperreal.dal.{BasicDAL, numberType, toBigInt}

import scala.collection.immutable.AbstractSeq
import java.{lang => boxed}

object VMRangeClass extends VMClass {
  val parent: VMClass = VMObjectClass
  val name: String = "Range"
  val extending: List[VMType] = List(parent)
  val members: Map[Symbol, VMMember] = Map()
  val clas: VMClass = VMClassClass
}

class VMRange(val range: IndexedSeq[Any])
    extends VMObject
    with VMNonResizableSequence
    with VMNonMap
    with VMNonUpdatable
    with VMUnordered
    with VMNonSet {
  def this(start: Number, end: Number, step: Number, inclusive: Boolean) =
    this(start match {
      case i: boxed.Integer if inclusive => i.toInt to end.intValue by step.intValue
      case i: boxed.Integer              => i.toInt until end.intValue by step.intValue
      case d: boxed.Double if inclusive =>
        BigDecimal(d) to end.doubleValue by step.doubleValue
      case d: boxed.Double =>
        BigDecimal(d) until end.doubleValue by step.doubleValue
      case i: BigInt if inclusive => i to toBigInt(end) by toBigInt(step)
      case i: BigInt              => i until toBigInt(end) by toBigInt(step)
      case d: BigDecimal if inclusive =>
        d to BasicDAL.toBigDecimal(end) by BasicDAL.toBigDecimal(step)
      case d: BigDecimal => d until BasicDAL.toBigDecimal(end) by BasicDAL.toBigDecimal(step)
      case v             =>
        /*problem(pf,*/
        sys.error(s"expected a number (real and not a fraction) as the range initial value: $start")
    })

  val clas: VMClass = VMRangeClass

  def iterator: Iterator[VMObject] =
    range.iterator.map(n => VMNumber(n.asInstanceOf[Number]))

  def apply(idx: VMObject): VMObject = VMNumber(range(idx.asInstanceOf[VMNumber].value.intValue).asInstanceOf[Number])

  def size: Int = range.length

  def append(elem: VMObject): VMObject = VMSeqClass.build(iterator ++ Iterator(elem))

  def concat(iterable: VMObject): VMObject = VMSeqClass.build(iterator ++ iterable.iterator)

  def head: VMObject = VMNumber(range.head.asInstanceOf[Number])

  def tail: VMObject = new VMRange(range.tail)

  override def toString: String = range.toString
}
