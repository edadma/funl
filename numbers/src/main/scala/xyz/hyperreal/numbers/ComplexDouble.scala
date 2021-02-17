package xyz.hyperreal.numbers

import math._

class ComplexDouble(val re: Double, val im: Double) extends AbstractComplexRational[Double, ComplexDouble] {

  protected def fractional(a: Double) = a

  protected def complex(re: Double, im: Double) = ComplexDouble(re, im)

  protected def promote = this

  protected def divide(a: Double, b: Double) = a / b

  def zero = ComplexDouble.zero

  def one = ComplexDouble.one

  def i = ComplexDouble.i

  def doubleValue = abs

  def floatValue = abs.toFloat

  def intValue = abs.toInt

  def longValue = abs.toLong

  def roughly(x: Double, y: Double) = (x - y).abs < 1e-13

  def roughly(that: ComplexDouble): Boolean =
    roughly(re, that.re) && roughly(im, that.im)

  def nearly(x: Double, y: Double) = (x - y).abs < 1e-15

  override def equals(o: Any) =
    o match {
      case r: ComplexDouble => nearly(im, r.im) && nearly(re, r.re)
      case r: Int           => nearly(im, 0) && nearly(re, r)
      case r: Double        => nearly(im, 0) && nearly(re, r)
      case (_: BigInt | _: BigDecimal | _: Rational) =>
        nearly(im, 0) && nearly(re, o.asInstanceOf[Number].doubleValue)
      case _ => false
    }

}

object ComplexDouble {

  def apply(re: Double, im: Double) = new ComplexDouble(re, im)

  val i: ComplexDouble = ComplexDouble(0, 1)

  val zero = ComplexDouble(0, 0)

  val one = ComplexDouble(1, 0)

  def apply(a: Double) = new ComplexDouble(a, 0)

  implicit def int2complex(a: Int) = ComplexDouble(a)

  implicit def double2complex(a: Double) = ComplexDouble(a)
}
