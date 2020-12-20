package xyz.hyperreal.dal

import java.math.{MathContext, RoundingMode}
import java.{lang => boxed}
import scala.math._
import xyz.hyperreal.numbers_jvm.{
  BigDecimalMath,
  ComplexBigDecimal,
  ComplexBigInt,
  ComplexDouble,
  ComplexRational,
  Rational
}
import BigDecimalMath.decimal128._

import scala.collection.mutable

object BasicDAL extends DAL {

  operation(
    Symbol("+"),
    IntType -> ((l: Number, r: Number) => maybePromote(l.longValue + r.longValue)),
    LongType -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) + toBigInt(r))),
    BigIntType -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) + toBigInt(r))),
    RationalType -> ((l: Number, r: Number) => maybeDemote(toRational(l) + toRational(r))),
    DoubleType -> ((l: Number, r: Number) => (DoubleType, l.doubleValue + r.doubleValue: Number)),
    BigDecType -> ((a: Number, b: Number) => (BigDecType, toBigDecimal(a) + toBigDecimal(b)))
  )

  operation(
    Symbol("-"),
    IntType -> ((l: Number, r: Number) => maybePromote(l.longValue - r.longValue)),
    LongType -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) - toBigInt(r))),
    BigIntType -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) - toBigInt(r))),
    RationalType -> ((l: Number, r: Number) => maybeDemote(toRational(l) - toRational(r))),
    DoubleType -> ((l: Number, r: Number) => (DoubleType, l.doubleValue - r.doubleValue: Number)),
    BigDecType -> ((a: Number, b: Number) => (BigDecType, toBigDecimal(a) - toBigDecimal(b)))
  )

  operation(
    Symbol("*"),
    IntType -> ((l: Number, r: Number) => maybePromote(l.longValue * r.longValue)),
    LongType -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) * toBigInt(r))),
    BigIntType -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) * toBigInt(r))),
    RationalType -> ((l: Number, r: Number) => maybeDemote(toRational(l) * toRational(r))),
    DoubleType -> ((l: Number, r: Number) => (DoubleType, l.doubleValue * r.doubleValue: Number)),
    BigDecType -> ((a: Number, b: Number) => (BigDecType, toBigDecimal(a) * toBigDecimal(b)))
  )

  operation(
    Symbol("/"),
    IntType -> ((l: Number, r: Number) => maybeDemote(toRational(l) / toRational(r))),
    LongType -> ((l: Number, r: Number) => maybeDemote(toRational(l) / toRational(r))),
    BigIntType -> ((l: Number, r: Number) => maybeDemote(toRational(l) / toRational(r))),
    RationalType -> ((l: Number, r: Number) => maybeDemote(toRational(l) / toRational(r))),
    DoubleType -> ((l: Number, r: Number) => (DoubleType, l.doubleValue / r.doubleValue: Number)),
    BigDecType -> ((a: Number, b: Number) => (BigDecType, toBigDecimal(a) / toBigDecimal(b)))
  )

  operation(
    Symbol("//"),
    IntType -> ((l: Number, r: Number) => (DoubleType, l.doubleValue / r.doubleValue: Number)),
    LongType -> ((l: Number, r: Number) => (DoubleType, l.doubleValue / r.doubleValue: Number)),
    BigIntType -> ((l: Number, r: Number) => (DoubleType, l.doubleValue / r.doubleValue: Number)),
    RationalType -> ((l: Number, r: Number) => (DoubleType, l.doubleValue / r.doubleValue: Number)),
    DoubleType -> ((l: Number, r: Number) => (DoubleType, l.doubleValue / r.doubleValue: Number)),
    BigDecType -> ((a: Number, b: Number) => (BigDecType, toBigDecimal(a) / toBigDecimal(b)))
  )

  operation(
    Symbol("^"),
    IntType -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) pow r.intValue)),
    LongType -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) pow r.intValue)),
    BigIntType -> ((l: Number, r: Number) => {
      r match {
        case i: boxed.Integer if i >= 0 => (BigIntType, toBigInt(l) pow i)
        case i: boxed.Integer           => (DoubleType, pow(l.doubleValue, i.toDouble): Number)
        case bi: BigInt if bi >= 0      => (DoubleType, bitIntPow(toBigInt(l), bi))
        case bi: BigInt                 => (DoubleType, pow(l.doubleValue, bi.toDouble): Number)
      }
    }),
    RationalType -> ((l: Number, r: Number) => {
      r match {
        case i: boxed.Integer => maybeDemote(toRational(l) ^ i)
        case bi: BigInt       => (RationalType, toRational(l) ^ bi)
        case _                => (DoubleType, pow(l.doubleValue, r.doubleValue): Number)
      }
    }),
    DoubleType -> ((l: Number, r: Number) => (DoubleType, pow(l.doubleValue, r.doubleValue): Number)),
//    BigDecType -> ((a: Number, b: Number) => (BigDecType, toBigDecimal(a) / toBigDecimal(b)))
  )

  operation(
    Symbol("mod"),
    IntType -> ((l: Number, r: Number) => (IntType, l.intValue % r.intValue: Number)),
    LongType -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) % toBigInt(r))),
    BigIntType -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) % toBigInt(r))),
    // todo: rational
    DoubleType -> ((l: Number, r: Number) => (DoubleType, l.doubleValue % r.doubleValue: Number)),
    BigDecType -> ((l: Number, r: Number) => (BigDecType, toBigDecimal(l) % toBigDecimal(r)))
  )

  relation(
    Symbol("="),
    IntType -> ((l: Number, r: Number) => boolean(l == r)),
    LongType -> ((l: Number, r: Number) => boolean(l.longValue == r.longValue)),
    BigIntType -> ((l: Number, r: Number) => boolean(toBigInt(l) == toBigInt(r))),
    RationalType -> ((l: Number, r: Number) => boolean(toRational(l) == toRational(r))),
    DoubleType -> ((l: Number, r: Number) => boolean(l.doubleValue == r.doubleValue)),
    BigDecType -> ((l: Number, r: Number) => boolean(toBigDecimal(l) == toBigDecimal(r)))
  )

  relation(
    Symbol("!="),
    IntType -> ((l: Number, r: Number) => boolean(l != r)),
    LongType -> ((l: Number, r: Number) => boolean(l.longValue != r.longValue)),
    BigIntType -> ((l: Number, r: Number) => boolean(toBigInt(l) != toBigInt(r))),
    RationalType -> ((l: Number, r: Number) => boolean(toRational(l) != toRational(r))),
    DoubleType -> ((l: Number, r: Number) => boolean(l.doubleValue != r.doubleValue)),
    BigDecType -> ((l: Number, r: Number) => boolean(toBigDecimal(l) != toBigDecimal(r)))
  )

  relation(
    Symbol("<"),
    IntType -> ((l: Number, r: Number) => boolean(l.intValue < r.intValue)),
    LongType -> ((l: Number, r: Number) => boolean(l.longValue < r.longValue)),
    BigIntType -> ((l: Number, r: Number) => boolean(toBigInt(l) < toBigInt(r))),
    RationalType -> ((l: Number, r: Number) => boolean(toRational(l) < toRational(r))),
    DoubleType -> ((l: Number, r: Number) => boolean(l.doubleValue < r.doubleValue)),
    BigDecType -> ((l: Number, r: Number) => boolean(toBigDecimal(l) < toBigDecimal(r)))
  )

  relation(
    Symbol(">"),
    IntType -> ((l: Number, r: Number) => boolean(l.intValue > r.intValue)),
    LongType -> ((l: Number, r: Number) => boolean(l.longValue > r.longValue)),
    BigIntType -> ((l: Number, r: Number) => boolean(toBigInt(l) > toBigInt(r))),
    RationalType -> ((l: Number, r: Number) => boolean(toRational(l) > toRational(r))),
    DoubleType -> ((l: Number, r: Number) => boolean(l.doubleValue > r.doubleValue)),
    BigDecType -> ((l: Number, r: Number) => boolean(toBigDecimal(l) > toBigDecimal(r)))
  )

  relation(
    Symbol("<="),
    IntType -> ((l: Number, r: Number) => boolean(l.intValue <= r.intValue)),
    LongType -> ((l: Number, r: Number) => boolean(l.longValue <= r.longValue)),
    BigIntType -> ((l: Number, r: Number) => boolean(toBigInt(l) <= toBigInt(r))),
    RationalType -> ((l: Number, r: Number) => boolean(toRational(l) <= toRational(r))),
    DoubleType -> ((l: Number, r: Number) => boolean(l.doubleValue <= r.doubleValue)),
    BigDecType -> ((l: Number, r: Number) => boolean(toBigDecimal(l) <= toBigDecimal(r)))
  )

  relation(
    Symbol(">="),
    IntType -> ((l: Number, r: Number) => boolean(l.intValue >= r.intValue)),
    LongType -> ((l: Number, r: Number) => boolean(l.longValue >= r.longValue)),
    BigIntType -> ((l: Number, r: Number) => boolean(toBigInt(l) >= toBigInt(r))),
    RationalType -> ((l: Number, r: Number) => boolean(toRational(l) >= toRational(r))),
    DoubleType -> ((l: Number, r: Number) => boolean(l.doubleValue >= r.doubleValue)),
    BigDecType -> ((l: Number, r: Number) => boolean(toBigDecimal(l) >= toBigDecimal(r)))
  )

  relation(
    Symbol("div"),
    IntType -> ((l, r) => boolean(r.intValue % l.intValue == 0)),
    LongType -> ((l: Number, r: Number) => boolean(r.longValue % l.longValue == 0)),
    BigIntType -> ((l: Number, r: Number) => boolean(toBigInt(r) % toBigInt(l) == 0)),
  )

  operation(
    Symbol("\\"),
    IntType -> ((l, r) => (IntType, r.intValue / l.intValue: Number)),
    LongType -> ((l: Number, r: Number) => (LongType, r.longValue / l.longValue: Number)),
    BigIntType -> ((l: Number, r: Number) => (BigIntType, toBigInt(r) / toBigInt(l))),
  )

  operation(
    Symbol("and"),
    IntType -> ((l: Number, r: Number) => (IntType, l.intValue & r.intValue: Number)),
    LongType -> ((l: Number, r: Number) => (IntType, l.longValue & r.longValue: Number)),
    BigIntType -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) & toBigInt(r)))
  )

  operation(
    Symbol("or"),
    IntType -> ((l: Number, r: Number) => (IntType, l.intValue | r.intValue: Number)),
    LongType -> ((l: Number, r: Number) => (IntType, l.longValue | r.longValue: Number)),
    BigIntType -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) | toBigInt(r)))
  )

  operation(
    Symbol("xor"),
    IntType -> ((l: Number, r: Number) => (IntType, l.intValue ^ r.intValue: Number)),
    LongType -> ((l: Number, r: Number) => (IntType, l.longValue ^ r.longValue: Number)),
    BigIntType -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) ^ toBigInt(r)))
  )

  operation(
    Symbol("compare"),
    IntType -> ((l: Number, r: Number) => (IntType, l.intValue compare r.intValue: Number)),
    LongType -> ((l: Number, r: Number) => (IntType, l.longValue compare r.longValue: Number)),
    BigIntType -> ((l: Number, r: Number) => (IntType, toBigInt(l) compare toBigInt(r): Number)),
    RationalType -> ((l: Number, r: Number) => (IntType, toRational(l) compare toRational(r): Number)),
    DoubleType -> ((l: Number, r: Number) => (IntType, l.doubleValue compare r.doubleValue: Number)),
    BigDecType -> ((l: Number, r: Number) => (IntType, toBigDecimal(l) compare toBigDecimal(r): Number))
  )

  def negate(typ: Type, n: Number): (Type, Number) =
    (typ, n) match {
      case (IntType, a: boxed.Integer)   => (IntType, -a)
      case (LongType, a: boxed.Long)     => (LongType, -a)
      case (BigIntType, a: BigInt)       => (BigIntType, -a)
      case (RationalType, a: Rational)   => (RationalType, -a)
      case (DoubleType, a: boxed.Double) => (DoubleType, -a)
      case (BigDecType, a: BigDecimal)   => (BigDecType, -a)
    }

  def negate(n: TypedNumber, number: ((Type, Number)) => TypedNumber = DALNumber.apply): TypedNumber =
    number(negate(n.typ, n.value))

  def invert(n: Number): (Type, Number) =
    n match {
      case a: boxed.Integer => maybePromote(~a & 0xFFFFFFFFL)
      case a: BigInt        => maybeDemote(~a)
    }

  def sqrtFunction(n: Any): Number =
    n match {
      case a: boxed.Integer =>
        val n = abs(a)
        val dr = sqrt(n.toDouble)
        val ir = round(dr).toInt

        if (ir * ir == n)
          if (a < 0) new ComplexBigInt(0, ir) else ir
        else if (a < 0) new ComplexDouble(0, dr)
        else dr
      case a: BigInt =>
        bisqrt(a.abs) match {
          case Left(ir)  => if (a < 0) new ComplexBigInt(0, ir) else maybeDemote(ir)._2
          case Right(dr) => if (a < 0) new ComplexDouble(0, dr.doubleValue) else dr
        }
      case a: Rational =>
        val ar = a.abs

        def rsqrt = if (a < 0) new ComplexDouble(0, sqrt(ar.doubleValue)) else sqrt(ar.doubleValue).asInstanceOf[Number]

        bisqrt(ar.n) match {
          case Left(irn) =>
            bisqrt(ar.d) match {
              case Left(ird) =>
                val res = Rational(irn, ird)

                if (a < 0) new ComplexRational(0, res) else res
              case _ => rsqrt
            }
          case _ => rsqrt
        }
      case a: boxed.Double      => if (a < 0) new ComplexDouble(0, sqrt(-a)) else sqrt(a)
      case a: BigDecimal        => if (a < 0) new ComplexBigDecimal(0, BigDecimalMath.sqrt(-a)) else BigDecimalMath.sqrt(a)
      case a: ComplexBigInt     => a.sqrt
      case a: ComplexRational   => a.sqrt
      case a: ComplexDouble     => a.sqrt
      case a: ComplexBigDecimal => a.sqrt
    }

  def absFunction(n: Any): Number =
    n match {
      case a: boxed.Integer                      => maybePromote(abs(a.longValue))._2
      case a: BigInt                             => maybeDemote(a.abs)._2
      case a: xyz.hyperreal.numbers_jvm.Rational => a.abs
      case a: boxed.Double                       => abs(a)
      case a: BigDecimal                         => a.abs
      case a: ComplexBigInt                      => a.abs
      case a: ComplexRational                    => a.abs
      case a: ComplexDouble                      => a.abs
      case a: ComplexBigDecimal                  => a.abs
    }

  def lnFunction(n: Any): Number =
    n match {
      case a: boxed.Integer                      => log(a.doubleValue)
      case a: BigInt                             => log(a.doubleValue)
      case a: xyz.hyperreal.numbers_jvm.Rational => log(a.doubleValue)
      case a: boxed.Double                       => log(a)
      case a: BigDecimal                         => BigDecimalMath.ln(a)
      case a: ComplexBigInt                      => a.ln
      case a: ComplexRational                    => a.ln
      case a: ComplexDouble                      => a.ln
      case a: ComplexBigDecimal                  => a.ln
    }

  def floorFunction(n: Any): Number =
    n match {
      case a: boxed.Integer                      => a
      case a: BigInt                             => maybeDemote(a)._2
      case a: xyz.hyperreal.numbers_jvm.Rational => a.floor
      case a: boxed.Double =>
        val f = BigDecimal(a).setScale(0, BigDecimal.RoundingMode.FLOOR)

        if (f.isValidInt)
          f.toIntExact
        else
          f.toBigInt
      case a: BigDecimal        => a.setScale(0, BigDecimal.RoundingMode.FLOOR)
      case a: ComplexBigInt     => a
      case a: ComplexRational   => a.floor
      case a: ComplexDouble     => a.floor
      case a: ComplexBigDecimal => a.floor
    }

  def ceilFunction(n: Any): Number =
    n match {
      case a: boxed.Integer                      => a
      case a: BigInt                             => maybeDemote(a)._2
      case a: xyz.hyperreal.numbers_jvm.Rational => a.ceil
      case a: boxed.Double =>
        val f = BigDecimal(a).setScale(0, BigDecimal.RoundingMode.CEILING)

        if (f.isValidInt)
          f.toIntExact
        else
          f.toBigInt
      case a: BigDecimal        => a.setScale(0, BigDecimal.RoundingMode.CEILING)
      case a: ComplexBigInt     => a
      case a: ComplexRational   => a.ceil
      case a: ComplexDouble     => a.ceil
      case a: ComplexBigDecimal => a.ceil
    }

  def cosFunction(n: Any): Number =
    n match {
      case _: boxed.Integer | _: BigInt | _: Rational | _: boxed.Double =>
        cos(n.asInstanceOf[Number].doubleValue).asInstanceOf[boxed.Double]
      case a: BigDecimal        => BigDecimalMath.cos(a)
      case a: ComplexBigInt     => a.cos
      case a: ComplexRational   => a.cos
      case a: ComplexDouble     => a.cos
      case a: ComplexBigDecimal => a.cos
    }

  def sinFunction(n: Any): Number =
    n match {
      case _: boxed.Integer | _: BigInt | _: Rational | _: boxed.Double =>
        sin(n.asInstanceOf[Number].doubleValue).asInstanceOf[boxed.Double]
      case a: BigDecimal        => BigDecimalMath.sin(a)
      case a: ComplexBigInt     => a.sin
      case a: ComplexRational   => a.sin
      case a: ComplexDouble     => a.sin
      case a: ComplexBigDecimal => a.sin
    }

  def tanFunction(n: Any): Number =
    n match {
      case _: boxed.Integer | _: BigInt | _: Rational | _: boxed.Double =>
        tan(n.asInstanceOf[Number].doubleValue).asInstanceOf[boxed.Double]
      //			case a: BigDecimal => BigDecimalMath.tan( a )
      case a: ComplexBigInt     => a.tan
      case a: ComplexRational   => a.tan
      case a: ComplexDouble     => a.tan
      case a: ComplexBigDecimal => a.tan
    }

  def acosFunction(n: Any): Number =
    n match {
      case _: boxed.Integer | _: BigInt | _: Rational | _: boxed.Double =>
        acos(n.asInstanceOf[Number].doubleValue).asInstanceOf[boxed.Double]
      case a: BigDecimal        => BigDecimalMath.acos(a)
      case a: ComplexBigInt     => a.acos
      case a: ComplexRational   => a.acos
      case a: ComplexDouble     => a.acos
      case a: ComplexBigDecimal => a.acos
    }

  def asinFunction(n: Any): Number =
    n match {
      case _: boxed.Integer | _: BigInt | _: Rational | _: boxed.Double =>
        asin(n.asInstanceOf[Number].doubleValue).asInstanceOf[boxed.Double]
      case a: BigDecimal        => BigDecimalMath.asin(a)
      case a: ComplexBigInt     => a.asin
      case a: ComplexRational   => a.asin
      case a: ComplexDouble     => a.asin
      case a: ComplexBigDecimal => a.asin
    }

  def atanFunction(n: Any): Number =
    n match {
      case _: boxed.Integer | _: BigInt | _: Rational | _: boxed.Double =>
        atan(n.asInstanceOf[Number].doubleValue).asInstanceOf[boxed.Double]
      case a: BigDecimal        => BigDecimalMath.atan(a)
      case a: ComplexBigInt     => a.atan
      case a: ComplexRational   => a.atan
      case a: ComplexDouble     => a.atan
      case a: ComplexBigDecimal => a.atan
    }

  def expFunction(n: Any): Number =
    n match {
      case _: boxed.Integer | _: BigInt | _: Rational | _: boxed.Double =>
        exp(n.asInstanceOf[Number].doubleValue).asInstanceOf[boxed.Double]
      case a: BigDecimal        => BigDecimalMath.exp(a)
      case a: ComplexBigInt     => a.exp
      case a: ComplexRational   => a.exp
      case a: ComplexDouble     => a.exp
      case a: ComplexBigDecimal => a.exp
    }

}

abstract class DAL(implicit var bdmath: BigDecimalMath) {

  protected val opmap = new mutable.HashMap[(Type, Symbol, Type), Operator]
  protected val specials = new mutable.HashMap[(Type, Type), Type]

  protected def mathContext(p: Int) = new MathContext(p, RoundingMode.HALF_EVEN)

  protected def bisqrt(n: BigInt): Either[BigInt, BigDecimal] = {
    val dr = BigDecimalMath.sqrt(bigDecimal(n))
    val ir = dr.toBigInt

    if (ir * ir == n)
      Left(ir)
    else
      Right(dr)
  }

  def bigDecimal(n: Int): BigDecimal = BigDecimal(n, bdmath.mc)

  def bigDecimal(n: Double): BigDecimal = BigDecimal(n, bdmath.mc)

  def bigDecimal(r: Rational): BigDecimal = r.decimalValue(bdmath.mc)

  def bigDecimal(n: BigInt): BigDecimal = {
    val len = digits(n)
    val m = if (len >= bdmath.mc.getPrecision) mathContext(len + 5) else bdmath.mc

    BigDecimal(n, m)
  }

  def decimal(s: String): Any = {
    val res = BigDecimal(s, bdmath.mc)

    if (res.isExactDouble) //isValidDouble)
      res.toDouble
    else
      res
  }

  def toBigDecimal(a: Number): BigDecimal =
    a match {
      case bd: BigDecimal   => bd
      case i: boxed.Integer => bigDecimal(i)
      case d: boxed.Double  => bigDecimal(d)
      case r: Rational      => bigDecimal(r)
      case bi: BigInt       => bigDecimal(bi)
      case _                => sys.error("can't convert from " + a)
    }

  protected def boolean(b: Boolean): (Type, Boolean) = (null, b)

  protected def toBigInt(a: Number): BigInt =
    a match {
      case bi: BigInt       => bi
      case i: boxed.Integer => BigInt(i)
      case _                => sys.error("can't convert from " + a)
    }

  protected def toRational(a: Number): Rational =
    a match {
      case r: Rational      => r
      case bi: BigInt       => Rational(bi)
      case i: boxed.Integer => Rational(i)
      case _                => sys.error("can't convert from " + a)
    }

  protected def maybeDemote(n: Double): (Type, Number) =
    if (n.isValidInt)
      (IntType, n.toInt.asInstanceOf[Number])
    else if (n.isWhole)
      (BigIntType, BigInt(n.toLong))
    else
      (DoubleType, n)

  protected def maybeDemote(n: BigDecimal): (Type, Number) =
    if (n.isValidInt)
      (IntType, n.toInt.asInstanceOf[Number])
    else if (n.isWhole)
      (BigIntType, n.toBigInt)
    else
      (BigDecType, n)

  protected def maybeDemote(n: BigInt): (Type, Number) =
    if (n.isValidInt)
      (IntType, n.toInt.asInstanceOf[Number])
    else
      (BigIntType, n)

  protected def maybeDemote(r: Rational): (Type, Number) =
    if (r.isInt) {
      if (r.n.isValidInt)
        (IntType, r.intValue.asInstanceOf[Number])
      else if (r.n.isValidLong)
        (LongType, r.longValue.asInstanceOf[Number])
      else
        (BigIntType, r.n)
    } else
      (RationalType, r)

  protected def maybePromote(n: Long): (Type, Number) =
    if (n.isValidInt)
      (IntType, n.toInt.asInstanceOf[Number])
    else
      (BigIntType, n)

  protected def resultant(rank: mutable.HashMap[Type, Int], l: Type, r: Type): Int =
    specials get (l, r) match {
      case Some(t) => rank(t)
      case None    => rank(l) max rank(r)
    }

  protected def operation(op: Symbol, funcs: (Type, Operator)*): Unit =
    define(op, funcs.asInstanceOf[Seq[(Type, Operator)]])

  protected def relation(op: Symbol, funcs: (Type, (Number, Number) => (Type, Boolean))*): Unit =
    define(op, funcs.asInstanceOf[Seq[(Type, Operator)]])

  protected def define(op: Symbol, funcs: Seq[(Type, Operator)]): Unit = {
    val rank = new mutable.HashMap[Type, Int]
    val types: Array[Type] = funcs.map(_._1).toArray

    for ((t, r) <- types.zipWithIndex)
      rank(t) = r

    val comb = types.combinations(2).toList map (s => (s.head, s(1)))
    val args = comb ++ comb.map(t => (t._2, t._1)) ++ types.map(t => (t, t))

    for ((l, r) <- args)
      opmap((l, op, r)) = funcs(resultant(rank, l, r))._2
  }

  def compare(x: Number, y: Number): Int = compute(Symbol("compare"), x, y).intValue

  def compute(op: String, left: Number, right: Number): Number = compute(Symbol(op), left, right)

  def compute(op: Symbol, left: Number, right: Number): Number =
    compute(op, DALNumber(left), DALNumber(right), DALNumber.apply).value

  def relate(op: String, left: Number, right: Number): Boolean = relate(Symbol(op), left, right)

  def relate(op: Symbol, left: Number, right: Number): Boolean = relate(op, DALNumber(left), DALNumber(right))

  def relate(op: Symbol, left: TypedNumber, right: TypedNumber): Boolean = {
    val (t, r) = opmap(left.typ, op, right.typ)(left.value, right.value)

    if (t != null)
      sys.error(s"operation '$op' is not a relation")
    else
      r.asInstanceOf[Boolean]
  }

  def compute[T <: TypedNumber](op: Symbol,
                                left: TypedNumber,
                                right: TypedNumber,
                                number: ((Type, Number)) => T /*= DALNumber.apply*/ ): T = {
    val n @ (t, r) = opmap(left.typ, op, right.typ)(left.value, right.value)

    if (t == null)
      sys.error(s"operation '$op' does not return number")
    else
      number(n.asInstanceOf[(Type, Number)])
  }

  def perform(op: Symbol, left: Number, right: Number): Any =
    opmap(DAL.numberType(left), op, DAL.numberType(right))(left, right)._2

  def perform(op: Symbol,
              left: TypedNumber,
              right: TypedNumber,
              number: ((Type, Number)) => TypedNumber = DALNumber.apply): Any =
    opmap(left.typ, op, right.typ)(left.value, right.value) match {
      case (null, b: java.lang.Boolean) => b
      case n                            => number(n.asInstanceOf[(Type, Number)])
    }

}

object DAL {

  def numberType(n: Number): Type =
    n match {
      case _: boxed.Integer => IntType
      case _: boxed.Long    => LongType
      case _: BigInt        => BigIntType
      case _: Rational      => RationalType
      case _: boxed.Double  => DoubleType
      case _: BigDecimal    => BigDecType
    }

}

case class DALNumber(typ: Type, value: Number) extends TypedNumber

object DALNumber {

  def apply(n: (Type, Number)): TypedNumber = DALNumber(n._1, n._2)

  def apply(n: Int) = new DALNumber(IntType, n)

  def apply(n: Long) = new DALNumber(LongType, n)

  def apply(n: BigInt) = new DALNumber(BigIntType, n)

  def apply(n: Rational) = new DALNumber(RationalType, n)

  def apply(n: Double) = new DALNumber(DoubleType, n)

  def apply(n: BigDecimal) = new DALNumber(BigDecType, n)

  def apply(n: Number): DALNumber = new DALNumber(DAL.numberType(n), n)

}

trait TypedNumber {
  val typ: Type
  val value: Number
}

abstract class Type
case object IntType extends Type
case object LongType extends Type
case object BigIntType extends Type
case object RationalType extends Type
case object DoubleType extends Type
case object BigDecType extends Type
