package xyz.hyperreal.numbers

import Numeric.Implicits._

abstract class Quaternion[T: Numeric, F: Fractional, Q <: Quaternion[T, F, Q, P], P <: Quaternion[F, F, P, P]] extends Number {

  val a: T
  val b: T
  val c: T
  val d: T

  protected def unsup: Nothing = sys.error("unsupported operation")

  protected def fractional(a: T): F

  protected def _floor(a: F): F

  protected def _ceil(a: F): F

  protected def _sqrt(a: F): F

  protected def _atan2(y: F, x: F): F

  protected def _ln(a: F): F

  protected def _exp(a: F): F

  protected def _sin(a: F): F

  protected def _cos(a: F): F

  protected def _acos(a: F): F

  protected def _pow(a: F, b: F): F

  protected def quaternion(a: T, b: T, c: T, e: T): Q

  protected def quaternion(a: T): Q = quaternion(a, implicitly[Numeric[T]].zero, implicitly[Numeric[T]].zero, implicitly[Numeric[T]].zero)

  protected def promote(a: F, b: F, c: F, d: F): P

  protected def promote(a: F): P = promote(a, implicitly[Numeric[F]].zero, implicitly[Numeric[F]].zero, implicitly[Numeric[F]].zero)

  protected def promote: P

  protected def divide(a: T, b: T): T

  protected def fdivide(a: F, b: F): F

  protected def fmul(a: F, b: F): F

  def zero: Q

  def one: Q

  def i: Q

  protected def ix: Q = i * this

  def tzero: T = implicitly[Numeric[T]].zero

  lazy val norm2: T = a * a + b * b + c * c + d * d

  lazy val norm: F = _sqrt(fractional(norm2))

  lazy val vnorm: F = _sqrt(fractional(b * b + c * c + d * d))

  def floor: P = promote(_floor(fractional(a)), _floor(fractional(b)), _floor(fractional(c)), _floor(fractional(d)))

  def ceil: P = promote(_ceil(fractional(a)), _ceil(fractional(b)), _ceil(fractional(c)), _ceil(fractional(d)))

  def sqrt: P =
    this ^ implicitly[Fractional[F]].div(implicitly[Fractional[F]].one, implicitly[Fractional[F]].fromInt(2))

  lazy val sgn: Q =
    if (this == zero)
      zero
    else
      this / norm

  lazy val arg: F = _acos(fdivide(fractional(a), norm))

  //protected implicit def int2T(n: Int): T = implicitly[Numeric[T]].fromInt(n)

  lazy val im: Q = (this - conj) / 2

  lazy val ln: P = promote(_ln(norm)) + im.sgn * arg

  def exp: P = promote(_exp(fractional(a))) * (promote(_cos(im.norm)) + im.sgn * _sin(im.norm))

  def sin: P =
    (ix.exp - (-ix).exp) / 2 / promote(implicitly[Numeric[F]].zero, implicitly[Numeric[F]].one)

  def asin: P = (-i).promote * (ix.promote + (one - this * this).sqrt).ln

  def sinh: P = (exp - (-this).exp) / 2

  def asinh: P = (promote + (this * this + 1).sqrt).ln

  def cos: P = (ix.exp + (-ix).exp) / 2

  def acos: P = i.promote * (promote - i.promote * (one - this * this).sqrt).ln

  def acosh: P = (promote + (this + 1).sqrt * (this - 1).sqrt).ln

  def cosh: P = (exp + (-this).exp) / 2

  def tan: P = ((ix * 2).exp - 1) / i.promote / ((ix * 2).exp + 1)

  def atan: P = i.promote * ((one - ix).ln - (one + ix).ln) / 2

  def tanh: P = (exp - (-this).exp) / (exp + (-this).exp)

  def atanh: P = ((one + this).ln - (one - this).ln) / 2

  def conj: Q = quaternion(a, -b, -c, -d)

  def ^(that: Q): P = (that.promote * ln).exp

  def ^(p: F): P = pow(p)

  def pow(p: F): P = {
    val c = promote
    val n = c.norm2
    val a = c.arg
    val pa = p * a

    import Fractional.Implicits._

    promote(_pow(n, p / implicitly[Fractional[F]].fromInt(2))) * promote(_cos(pa), _sin(pa))
  }

  def ^(e: Int): Q

  def ^(e: BigInt): Q

  def +(that: Q): Q = quaternion(a + that.a, b + that.b, c + that.c, d + that.d)

  def +(that: T): Q = quaternion(a + that, b, c, d)

  def +(that: F): P = promote(fractional(a) + that, fractional(b), fractional(c), fractional(d))

  def +(that: Int): Q = quaternion(a + implicitly[Numeric[T]].fromInt(that), b, c, d)

  def *(that: Quaternion[T, F, Q, P]): Q =
    quaternion(a * that.a - im * that.im, im * that.re + a * that.im)

  def *(that: T): Q = quaternion(a * that, b * that, c * that, d * that)

  def *(that: F): P = promote(fractional(a) * that, fractional(b) * that, fractional(c) * that, fractional(d) * that)

  def *(that: Int): Q = {
    val n = implicitly[Numeric[T]].fromInt(that)

    quaternion(a * n, b * n, c * n, d * n)
  }

  def -(that: Quaternion[T, F, Q, P]): Q = quaternion(a - that.a, b - that.b, c - that.c, d - that.d)

  def -(that: T): Q = quaternion(a - that, im)

  def -(that: Int): Q = quaternion(a - implicitly[Numeric[T]].fromInt(that), im)

  def /(that: Quaternion[T, F, Q, P]): Q =
    quaternion(divide(a * that.re + im * that.im, that.norm2), divide(im * that.re - a * that.im, that.norm2))

  def /(that: T): Q = quaternion(divide(a, that), divide(im, that))

  def /(that: Int): Q =
    quaternion(divide(a, implicitly[Numeric[T]].fromInt(that)), divide(im, implicitly[Numeric[T]].fromInt(that)))

  def unary_- : Q = quaternion(-a, -b, -c, -d)

  def inverse: Q = conj / norm2

  override def equals(o: Any): Boolean =
    o match {
      case q: Quaternion[T, F, Q, P] => a == q.a && b == q.b && c == q.c && d == q.d
      case _                         => false
    }

  override def hashCode: Int = a.hashCode ^ b.hashCode ^ c.hashCode ^ d.hashCode

  override def toString: String = {
    val zero = implicitly[Numeric[T]].zero
    val one = implicitly[Numeric[T]].one

    if (im == zero)
      a.toString
    else if (a == zero) {
      if (im == one)
        "i"
      else if (im == -one)
        "-i"
      else
        s"${im}i"
    } else if (im == one)
      s"$a+i"
    else if (im == -one)
      s"$a-i"
    else if (implicitly[Numeric[T]].lt(im, zero))
      s"$a${im}i"
    else
      s"$a+${im}i"
  }

}
