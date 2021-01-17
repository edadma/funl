package xyz.hyperreal.numbers

class ComplexBigInt(val re: BigInt, val im: BigInt) extends AbstractComplexRational[BigInt, ComplexBigInt] {

  protected def fractional(a: BigInt) = a.doubleValue

  protected def complex(re: BigInt, im: BigInt) = ComplexBigInt(re, im)

  protected def promote = ComplexDouble(re.doubleValue, im.doubleValue)

  protected def divide(a: BigInt, b: BigInt) = unsup

  def zero = ComplexBigInt.zero

  def one = ComplexBigInt.one

  def i = ComplexBigInt.i

  def doubleValue = abs.toDouble

  def floatValue = abs.toFloat

  def intValue = abs.toInt

  def longValue = abs.toLong

  override def equals(o: Any) =
    o match {
      case r: Int    => im == 0 && re == r
      case r: BigInt => im == 0 && re == r
      case _         => this == o
    }

}

object ComplexBigInt {

  def apply(re: BigInt, im: BigInt) = new ComplexBigInt(re, im)

  val i = ComplexBigInt(0, 1)

  val zero = ComplexBigInt(0, 0)

  val one = ComplexBigInt(1, 0)

  def apply(a: BigInt) = new ComplexBigInt(a, 0)

  implicit def int2complex(a: Int) = ComplexBigInt(a)

  implicit def bigint2complex(a: BigInt) = ComplexBigInt(a)

}
