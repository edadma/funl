package xyz.hyperreal.numbers

case class ComplexBigDecimal(re: BigDecimal, im: BigDecimal)(implicit val bdmath: BigDecimalMath)
    extends Complex[BigDecimal, BigDecimal, ComplexBigDecimal, ComplexBigDecimal] {

  protected def promote(re: BigDecimal, im: BigDecimal) =
    ComplexBigDecimal(re, im)

  protected def _floor(a: BigDecimal) =
    a.setScale(0, BigDecimal.RoundingMode.FLOOR)

  protected def _ceil(a: BigDecimal) =
    a.setScale(0, BigDecimal.RoundingMode.CEILING)

  protected def _sqrt(a: BigDecimal) = BigDecimalMath.sqrt(a)

  protected def _atan2(y: BigDecimal, x: BigDecimal) =
    BigDecimalMath.atan2(y, x)

  protected def _ln(a: BigDecimal) = BigDecimalMath.ln(a)

  protected def _exp(a: BigDecimal) = BigDecimalMath.exp(a)

  protected def _sin(a: BigDecimal) = BigDecimalMath.sin(a)

  protected def _cos(a: BigDecimal) = BigDecimalMath.cos(a)

  protected def _pow(a: BigDecimal, b: BigDecimal) = BigDecimalMath.pow(a, b)

  protected def fractional(a: BigDecimal) = a

  protected def complex(re: BigDecimal, im: BigDecimal) =
    ComplexBigDecimal(re, im)

  protected def promote = this

  protected def divide(a: BigDecimal, b: BigDecimal) = a / b

  def ^(p: Int) = ^(complex(p))

  def ^(p: BigInt) = ^(complex(BigDecimal(p)))

  def zero = bdmath.ZEROC.v

  def one = bdmath.ONEC.v

  def i = bdmath.I.v

  def doubleValue = abs.toDouble

  def floatValue = abs.toFloat

  def intValue = abs.toInt

  def longValue = abs.toLong

}

object ComplexBigDecimal {

  def i(implicit bdmath: BigDecimalMath) = ComplexBigDecimal(0, 1)

  def apply(a: BigDecimal)(implicit bdmath: BigDecimalMath) =
    new ComplexBigDecimal(a, 0)

  implicit def int2complex(a: Int)(implicit bdmath: BigDecimalMath) =
    ComplexBigDecimal(a)

  implicit def bigDecimal2complex(a: BigDecimal)(implicit bdmath: BigDecimalMath) = ComplexBigDecimal(a)
}
