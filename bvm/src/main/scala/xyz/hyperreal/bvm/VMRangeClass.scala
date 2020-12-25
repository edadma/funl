package xyz.hyperreal.bvm

import xyz.hyperreal.dal.{BasicDAL, numberType, toBigInt}

import scala.collection.immutable.AbstractSeq
import java.{lang => boxed}

object VMRangeClass extends VMClass {
  val parent: VMClass = VMObjectClass
  val name: String = "Range"
  val extending: List[VMType] = List(parent)
  val members: Map[Symbol, VMMember] = Map()
}

class VMRangeObject(start: Number, end: Number, step: Number, inclusive: Boolean) extends VMObject with VMIterable {
  val clas: VMClass = VMRangeClass
  val range: AbstractSeq[Any] with IndexedSeq[Any] =
    start match {
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
    }

  override def iterator: Iterator[VMObject] =
    range.iterator.map(n => VMNumber((numberType(n.asInstanceOf[Number]), n.asInstanceOf[Number])))

  override def toString: String = range.toString
}