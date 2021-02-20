package xyz.hyperreal.numbers

case class QuaternionInt(re: Int, im: Int) extends AbstractComplexRational[Int, QuaternionInt] {

  protected def fractional(a: Int): Double = a.toDouble

  protected def complex(re: Int, im: Int): QuaternionInt = QuaternionInt(re, im)

  protected def promote: ComplexDouble = ComplexDouble(re, im)

  protected def divide(a: Int, b: Int): Int = unsup

  def zero: QuaternionInt = QuaternionInt.zero

  def one: QuaternionInt = QuaternionInt.one

  def i: QuaternionInt = QuaternionInt.i

  def doubleValue: Double = abs

  def floatValue: Float = abs.toFloat

  def intValue: Int = abs.toInt

  def longValue: Long = abs.toLong

}

object QuaternionInt {

  val i: QuaternionInt = QuaternionInt(0, 1)

  val zero: QuaternionInt = QuaternionInt(0, 0)

  val one: QuaternionInt = QuaternionInt(1, 0)

  def apply(a: Int) = new QuaternionInt(a, 0)

  implicit def int2complex(a: Int): QuaternionInt = QuaternionInt(a)

}
