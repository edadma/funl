package xyz.hyperreal.numbers

case class ComplexInt(re: Int, im: Int) extends AbstractComplexRational[Int, ComplexInt] {

  protected def fractional(a: Int) = a.toDouble

  protected def complex(re: Int, im: Int) = ComplexInt(re, im)

  protected def promote = ComplexDouble(re, im)

  protected def divide(a: Int, b: Int) = unsup

  def zero = ComplexInt.zero

  def one = ComplexInt.one

  def i = ComplexInt.i

  def doubleValue = abs

  def floatValue = abs.toFloat

  def intValue = abs.toInt

  def longValue = abs.toLong

}

object ComplexInt {

  val i = ComplexInt(0, 1)

  val zero = ComplexInt(0, 0)

  val one = ComplexInt(1, 0)

  def apply(a: Int) = new ComplexInt(a, 0)

  implicit def int2complex(a: Int) = ComplexInt(a)

}
