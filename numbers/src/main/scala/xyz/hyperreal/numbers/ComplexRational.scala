package xyz.hyperreal.numbers

class ComplexRational(val re: Rational, val im: Rational) extends AbstractComplexRational[Rational, ComplexRational] {

  protected def fractional(a: Rational) = a.doubleValue

  protected def complex(re: Rational, im: Rational) = ComplexRational(re, im)

  protected def promote = ComplexDouble(re.doubleValue, im.doubleValue)

  protected def divide(a: Rational, b: Rational) = a / b

  def zero = ComplexRational.zero

  def one = ComplexRational.one

  def i = ComplexRational.i

  def doubleValue = abs.doubleValue

  def floatValue = abs.floatValue

  def intValue = abs.intValue

  def longValue = abs.longValue

  override def equals(o: Any): Boolean =
    o match {
      case r: Int      => im == 0 && re == r
      case r: BigInt   => im == 0 && re == r
      case r: Rational => im == 0 && re == r
      case _           => this == o
    }

}

object ComplexRational {

  def apply(re: Rational, im: Rational) = new ComplexRational(re, im)

  val i = ComplexRational(0, 1)

  val zero = ComplexRational(0, 0)

  val one = ComplexRational(1, 0)

  def apply(a: Rational) = new ComplexRational(a, 0)

  implicit def int2complex(a: Int) = ComplexRational(a)

  implicit def rational2complex(a: Rational) = ComplexRational(a)

}
