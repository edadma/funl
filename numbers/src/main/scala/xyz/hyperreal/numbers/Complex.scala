package xyz.hyperreal.numbers

import Numeric.Implicits._

abstract class Complex[T: Numeric, F: Fractional, C <: Complex[T, F, C, P], P <: Complex[F, F, P, P]] extends Number {

  val re: T

  val im: T

  protected def unsup = sys.error("unsupported operation")

  protected def fractional(a: T): F

  protected def _floor(a: F): F

  protected def _ceil(a: F): F

  protected def _sqrt(a: F): F

  protected def _atan2(y: F, x: F): F

  protected def _ln(a: F): F

  protected def _exp(a: F): F

  protected def _sin(a: F): F

  protected def _cos(a: F): F

  protected def _pow(a: F, b: F): F

  protected def complex(re: T, im: T): C

  protected def complex(re: T): C = complex(re, implicitly[Numeric[T]].zero)

  protected def promote(re: F, im: F): P

  protected def promote(re: F): P = promote(re, implicitly[Numeric[F]].zero)

  protected def promote: P

  protected def divide(a: T, b: T): T

  def zero: C

  def one: C

  def i: C

  protected def ix = i * this

  def norm: T = re * re + im * im

  def abs = _sqrt(fractional(norm))

  def floor = promote(_floor(fractional(re)), _floor(fractional(im)))

  def ceil = promote(_ceil(fractional(re)), _ceil(fractional(im)))

  def arg = _atan2(fractional(im), fractional(re))

  def sqrt: P =
    this ^ implicitly[Fractional[F]].div(implicitly[Fractional[F]].one, implicitly[Fractional[F]].fromInt(2))

  def ln = promote(_ln(abs), arg)

  def exp =
    promote(_exp(fractional(re))) * promote(_cos(fractional(im)), _sin(fractional(im)))

  def sin =
    (ix.exp - (-ix).exp) / 2 / promote(implicitly[Numeric[F]].zero, implicitly[Numeric[F]].one)

  def asin = (-i).promote * (ix.promote + (one - this * this).sqrt).ln

  def sinh = (exp - (-this).exp) / 2

  def asinh = (promote + (this * this + 1).sqrt).ln

  def cos = (ix.exp + (-ix).exp) / 2

  def acos = i.promote * (promote - i.promote * (one - this * this).sqrt).ln

  def acosh = (promote + (this + 1).sqrt * (this - 1).sqrt).ln

  def cosh = (exp + (-this).exp) / 2

  def tan = ((ix * 2).exp - 1) / i.promote / ((ix * 2).exp + 1)

  def atan = i.promote * ((one - ix).ln - (one + ix).ln) / 2

  def tanh = (exp - (-this).exp) / (exp + (-this).exp)

  def atanh = ((one + this).ln - (one - this).ln) / 2

  def conj = complex(re, -im)

  def ^(that: Complex[T, F, C, P]) = (that.promote * ln).exp

  def ^(p: F) = pow(p)

  def pow(p: F) = {
    val c = promote
    val n = c.norm
//		val lnn = _ln( n )
    val a = c.arg
    val pa = p * a

    import Fractional.Implicits._

    promote(_pow(n, p / implicitly[Fractional[F]].fromInt(2))) * promote(_cos(pa), _sin(pa))
  }

  def ^(e: Int): C

  def ^(e: BigInt): C

  def +(that: Complex[T, F, C, P]) = complex(re + that.re, im + that.im)

  def +(that: T) = complex(re + that, im)

  def +(that: Int) = complex(re + implicitly[Numeric[T]].fromInt(that), im)

  def *(that: Complex[T, F, C, P]) =
    complex(re * that.re - im * that.im, im * that.re + re * that.im)

  def *(that: T) = complex(re * that, im * that)

  def *(that: Int) =
    complex(re * implicitly[Numeric[T]].fromInt(that), im * implicitly[Numeric[T]].fromInt(that))

  def -(that: Complex[T, F, C, P]) = complex(re - that.re, im - that.im)

  def -(that: T) = complex(re - that, im)

  def -(that: Int) = complex(re - implicitly[Numeric[T]].fromInt(that), im)

  def /(that: Complex[T, F, C, P]) =
    complex(divide(re * that.re + im * that.im, that.norm), divide(im * that.re - re * that.im, that.norm))

  def /(that: T) = complex(divide(re, that), divide(im, that))

  def /(that: Int): C =
    complex(divide(re, implicitly[Numeric[T]].fromInt(that)), divide(im, implicitly[Numeric[T]].fromInt(that)))

  def unary_- : C = complex(-re, -im)

  def inverse: C = conj / norm

  override def equals(o: Any): Boolean =
    o match {
      case z: Complex[T, F, C, P] => re == z.re && im == z.im
      case _                      => false
    }

  override def hashCode: Int = re.hashCode ^ im.hashCode

  override def toString: String = {
    val zero = implicitly[Numeric[T]].zero
    val one = implicitly[Numeric[T]].one

    if (im == zero)
      re.toString
    else if (re == zero) {
      if (im == one)
        "i"
      else if (im == -one)
        "-i"
      else
        s"${im}i"
    } else if (im == one)
      s"$re+i"
    else if (im == -one)
      s"$re-i"
    else if (implicitly[Numeric[T]].lt(im, zero))
      s"$re${im}i"
    else
      s"$re+${im}i"
  }

}
