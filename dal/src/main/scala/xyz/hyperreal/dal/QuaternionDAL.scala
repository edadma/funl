package xyz.hyperreal.dal

import xyz.hyperreal.numbers.BigDecimalMath
import xyz.hyperreal.numbers.BigDecimalMath.decimal128._

import java.{lang => boxed}
import scala.math.{BigInt, pow}

object QuaternionDAL extends DAL {

  special(DoubleType, ComplexBigIntType, ComplexDoubleType)
  special(BigDecType, ComplexBigIntType, ComplexBigDecType)
  special(RationalType, ComplexBigIntType, ComplexRationalType)
  special(DoubleType, ComplexRationalType, ComplexDoubleType)
  special(BigDecType, ComplexRationalType, ComplexBigDecType)
  special(BigDecType, ComplexDoubleType, ComplexBigDecType)

  special(DoubleType, QuaternionBigIntType, QuaternionDoubleType)
  special(BigDecType, QuaternionBigIntType, QuaternionBigDecType)
  special(RationalType, QuaternionBigIntType, QuaternionRationalType)
  special(DoubleType, QuaternionRationalType, QuaternionDoubleType)
  special(BigDecType, QuaternionRationalType, QuaternionBigDecType)
  special(BigDecType, QuaternionDoubleType, QuaternionBigDecType)

  special(ComplexDoubleType, QuaternionBigIntType, QuaternionDoubleType)
  special(ComplexBigDecType, QuaternionBigIntType, QuaternionBigDecType)
  special(ComplexRationalType, QuaternionBigIntType, QuaternionRationalType)
  special(ComplexDoubleType, QuaternionRationalType, QuaternionDoubleType)
  special(ComplexBigDecType, QuaternionRationalType, QuaternionBigDecType)
  special(ComplexBigDecType, QuaternionDoubleType, QuaternionBigDecType)

  operation(
    Symbol("+"),
    IntType -> ((l: Number, r: Number) => maybePromote(l.longValue + r.longValue)),
    LongType -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) + toBigInt(r))),
    BigIntType -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) + toBigInt(r))),
    RationalType -> ((l: Number, r: Number) => maybeDemote(toRational(l) + toRational(r))),
    DoubleType -> ((l: Number, r: Number) => (DoubleType, l.doubleValue + r.doubleValue: Number)),
    BigDecType -> ((a: Number, b: Number) => (BigDecType, toBigDecimal(a) + toBigDecimal(b))),
    ComplexBigIntType -> ((l: Number, r: Number) => maybeDemote(toComplexBigInt(l) + toComplexBigInt(r))),
    ComplexRationalType -> ((l: Number, r: Number) => maybeDemote(toComplexRational(l) + toComplexRational(r))),
    ComplexDoubleType -> ((l: Number, r: Number) => (ComplexDoubleType, toComplexDouble(l) + toComplexDouble(r))),
    ComplexBigDecType -> ((l: Number, r: Number) => (ComplexBigDecType, toComplexBigDecimal(l) + toComplexBigDecimal(r))),
    QuaternionBigIntType -> ((l: Number, r: Number) => maybeDemote(toQuaternionBigInt(l) + toQuaternionBigInt(r))),
    QuaternionRationalType -> ((l: Number, r: Number) => maybeDemote(toQuaternionRational(l) + toQuaternionRational(r))),
    QuaternionDoubleType -> ((l: Number, r: Number) => (QuaternionDoubleType, toQuaternionDouble(l) + toQuaternionDouble(r))),
    QuaternionBigDecType -> ((l: Number, r: Number) => (QuaternionBigDecType, toQuaternionBigDecimal(l) + toQuaternionBigDecimal(r)))
  )

  operation(
    Symbol("-"),
    IntType -> ((l: Number, r: Number) => maybePromote(l.longValue - r.longValue)),
    LongType -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) - toBigInt(r))),
    BigIntType -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) - toBigInt(r))),
    RationalType -> ((l: Number, r: Number) => maybeDemote(toRational(l) - toRational(r))),
    DoubleType -> ((l: Number, r: Number) => (DoubleType, l.doubleValue - r.doubleValue: Number)),
    BigDecType -> ((a: Number, b: Number) => (BigDecType, toBigDecimal(a) - toBigDecimal(b))),
    ComplexBigIntType -> ((l: Number, r: Number) => maybeDemote(toComplexBigInt(l) - toComplexBigInt(r))),
    ComplexRationalType -> ((l: Number, r: Number) => maybeDemote(toComplexRational(l) - toComplexRational(r))),
    ComplexDoubleType -> ((l: Number, r: Number) => (ComplexDoubleType, toComplexDouble(l) - toComplexDouble(r))),
    ComplexBigDecType -> ((l: Number, r: Number) => (ComplexBigDecType, toComplexBigDecimal(l) - toComplexBigDecimal(r))),
    QuaternionBigIntType -> ((l: Number, r: Number) => maybeDemote(toQuaternionBigInt(l) - toQuaternionBigInt(r))),
    QuaternionRationalType -> ((l: Number, r: Number) => maybeDemote(toQuaternionRational(l) - toQuaternionRational(r))),
    QuaternionDoubleType -> ((l: Number, r: Number) => (QuaternionDoubleType, toQuaternionDouble(l) - toQuaternionDouble(r))),
    QuaternionBigDecType -> ((l: Number, r: Number) => (QuaternionBigDecType, toQuaternionBigDecimal(l) - toQuaternionBigDecimal(r)))
  )

  operation(
    Symbol("*"),
    IntType -> ((l: Number, r: Number) => maybePromote(l.longValue * r.longValue)),
    LongType -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) * toBigInt(r))),
    BigIntType -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) * toBigInt(r))),
    RationalType -> ((l: Number, r: Number) => maybeDemote(toRational(l) * toRational(r))),
    DoubleType -> ((l: Number, r: Number) => (DoubleType, l.doubleValue * r.doubleValue: Number)),
    BigDecType -> ((a: Number, b: Number) => (BigDecType, toBigDecimal(a) * toBigDecimal(b))),
    ComplexBigIntType -> ((l: Number, r: Number) => maybeDemote(toComplexBigInt(l) * toComplexBigInt(r))),
    ComplexRationalType -> ((l: Number, r: Number) => maybeDemote(toComplexRational(l) * toComplexRational(r))),
    ComplexDoubleType -> ((l: Number, r: Number) => (ComplexDoubleType, toComplexDouble(l) * toComplexDouble(r))),
    ComplexBigDecType -> ((l: Number, r: Number) => (ComplexBigDecType, toComplexBigDecimal(l) * toComplexBigDecimal(r))),
    QuaternionBigIntType -> ((l: Number, r: Number) => maybeDemote(toQuaternionBigInt(l) * toQuaternionBigInt(r))),
    QuaternionRationalType -> ((l: Number, r: Number) => maybeDemote(toQuaternionRational(l) * toQuaternionRational(r))),
    QuaternionDoubleType -> ((l: Number, r: Number) => (QuaternionDoubleType, toQuaternionDouble(l) * toQuaternionDouble(r))),
    QuaternionBigDecType -> ((l: Number, r: Number) => (QuaternionBigDecType, toQuaternionBigDecimal(l) * toQuaternionBigDecimal(r)))
  )

  operation(
    Symbol("/"),
    IntType -> ((l: Number, r: Number) => maybeDemote(toRational(l) / toRational(r))),
    LongType -> ((l: Number, r: Number) => maybeDemote(toRational(l) / toRational(r))),
    BigIntType -> ((l: Number, r: Number) => maybeDemote(toRational(l) / toRational(r))),
    RationalType -> ((l: Number, r: Number) => maybeDemote(toRational(l) / toRational(r))),
    DoubleType -> ((l: Number, r: Number) => (DoubleType, l.doubleValue / r.doubleValue: Number)),
    BigDecType -> ((a: Number, b: Number) => (BigDecType, toBigDecimal(a) / toBigDecimal(b))),
    ComplexBigIntType -> ((l: Number, r: Number) => maybeDemote(toComplexRational(l) / toComplexRational(r))),
    ComplexRationalType -> ((l: Number, r: Number) => maybeDemote(toComplexRational(l) / toComplexRational(r))),
    ComplexDoubleType -> ((l: Number, r: Number) => (ComplexDoubleType, toComplexDouble(l) / toComplexDouble(r))),
    ComplexBigDecType -> ((l: Number, r: Number) => (ComplexBigDecType, toComplexBigDecimal(l) / toComplexBigDecimal(r))),
    QuaternionBigIntType -> ((l: Number, r: Number) => maybeDemote(toQuaternionRational(l) / toQuaternionRational(r))),
    QuaternionRationalType -> ((l: Number, r: Number) => maybeDemote(toQuaternionRational(l) / toQuaternionRational(r))),
    QuaternionDoubleType -> ((l: Number, r: Number) => (QuaternionDoubleType, toQuaternionDouble(l) / toQuaternionDouble(r))),
    QuaternionBigDecType -> ((l: Number, r: Number) => (QuaternionBigDecType, toQuaternionBigDecimal(l) / toQuaternionBigDecimal(r)))
  )

  operation(
    Symbol("//"),
    IntType -> ((l: Number, r: Number) => (DoubleType, l.doubleValue / r.doubleValue: Number)),
    LongType -> ((l: Number, r: Number) => (DoubleType, l.doubleValue / r.doubleValue: Number)),
    BigIntType -> ((l: Number, r: Number) => (DoubleType, l.doubleValue / r.doubleValue: Number)),
    RationalType -> ((l: Number, r: Number) => (DoubleType, l.doubleValue / r.doubleValue: Number)),
    DoubleType -> ((l: Number, r: Number) => (DoubleType, l.doubleValue / r.doubleValue: Number)),
    BigDecType -> ((a: Number, b: Number) => (BigDecType, toBigDecimal(a) / toBigDecimal(b))),
    ComplexBigIntType -> ((l: Number, r: Number) => (ComplexDoubleType, toComplexDouble(l) / toComplexDouble(r))),
    ComplexRationalType -> ((l: Number, r: Number) => (ComplexDoubleType, toComplexDouble(l) / toComplexDouble(r))),
    ComplexDoubleType -> ((l: Number, r: Number) => (ComplexDoubleType, toComplexDouble(l) / toComplexDouble(r))),
    ComplexBigDecType -> ((l: Number, r: Number) => (ComplexBigDecType, toComplexBigDecimal(l) / toComplexBigDecimal(r))),
    QuaternionBigIntType -> ((l: Number, r: Number) => (QuaternionDoubleType, toQuaternionDouble(l) / toQuaternionDouble(r))),
    QuaternionRationalType -> ((l: Number, r: Number) => (QuaternionDoubleType, toQuaternionDouble(l) / toQuaternionDouble(r))),
    QuaternionDoubleType -> ((l: Number, r: Number) => (QuaternionDoubleType, toQuaternionDouble(l) / toQuaternionDouble(r))),
    QuaternionBigDecType -> ((l: Number, r: Number) => (QuaternionBigDecType, toQuaternionBigDecimal(l) / toQuaternionBigDecimal(r))),
  )

  operation(
    Symbol("^"),
    IntType -> ((l: Number, r: Number) => bigIntPow(l, r)),
    LongType -> ((l: Number, r: Number) => bigIntPow(l, r)),
    BigIntType -> ((l: Number, r: Number) => bigIntPow(l, r)),
    RationalType -> ((l: Number, r: Number) => {
      r match {
        case i: boxed.Integer => maybeDemote(toRational(l) ^ i)
        case bi: BigInt       => (RationalType, toRational(l) ^ bi)
        case _                => (DoubleType, pow(l.doubleValue, r.doubleValue): Number)
      }
    }),
    DoubleType -> ((l: Number, r: Number) => (DoubleType, pow(l.doubleValue, r.doubleValue): Number)),
    BigDecType -> ((l: Number, r: Number) => (BigDecType, BigDecimalMath.pow(toBigDecimal(l), toBigDecimal(r)))),
    ComplexBigIntType -> ((l: Number, r: Number) => (ComplexDoubleType, toComplexDouble(l) ^ toComplexDouble(r))),
    ComplexRationalType -> ((l: Number, r: Number) => (ComplexDoubleType, toComplexDouble(l) ^ toComplexDouble(r))),
    ComplexDoubleType -> ((l: Number, r: Number) => (ComplexDoubleType, toComplexDouble(l) ^ toComplexDouble(r))),
    ComplexBigDecType -> ((l: Number, r: Number) => (ComplexBigDecType, toComplexBigDecimal(l) ^ toComplexBigDecimal(r))),
    QuaternionBigIntType -> ((l: Number, r: Number) => (QuaternionDoubleType, toQuaternionDouble(l) ^ toQuaternionDouble(r))),
    QuaternionRationalType -> ((l: Number, r: Number) => (QuaternionDoubleType, toQuaternionDouble(l) ^ toQuaternionDouble(r))),
    QuaternionDoubleType -> ((l: Number, r: Number) => (QuaternionDoubleType, toQuaternionDouble(l) ^ toQuaternionDouble(r))),
    QuaternionBigDecType -> ((l: Number, r: Number) => (QuaternionBigDecType, toQuaternionBigDecimal(l) ^ toQuaternionBigDecimal(r))),
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
    BigDecType -> ((l: Number, r: Number) => boolean(toBigDecimal(l) == toBigDecimal(r))),
    ComplexBigIntType -> ((l: Number, r: Number) => boolean(toComplexBigInt(l) == toComplexBigInt(r))),
    ComplexRationalType -> ((l: Number, r: Number) => boolean(toComplexRational(l) == toComplexRational(r))),
    ComplexDoubleType -> ((l: Number, r: Number) => boolean(toComplexDouble(l) == toComplexDouble(r))),
    ComplexBigDecType -> ((l: Number, r: Number) => boolean(toComplexBigDecimal(l) == toComplexBigDecimal(r))),
    QuaternionBigIntType -> ((l: Number, r: Number) => boolean(toQuaternionBigInt(l) == toQuaternionBigInt(r))),
    QuaternionRationalType -> ((l: Number, r: Number) => boolean(toQuaternionRational(l) == toQuaternionRational(r))),
    QuaternionDoubleType -> ((l: Number, r: Number) => boolean(toQuaternionDouble(l) == toQuaternionDouble(r))),
    QuaternionBigDecType -> ((l: Number, r: Number) => boolean(toQuaternionBigDecimal(l) == toQuaternionBigDecimal(r)))
  )

  relation(
    Symbol("!="),
    IntType -> ((l: Number, r: Number) => boolean(l != r)),
    LongType -> ((l: Number, r: Number) => boolean(l.longValue != r.longValue)),
    BigIntType -> ((l: Number, r: Number) => boolean(toBigInt(l) != toBigInt(r))),
    RationalType -> ((l: Number, r: Number) => boolean(toRational(l) != toRational(r))),
    DoubleType -> ((l: Number, r: Number) => boolean(l.doubleValue != r.doubleValue)),
    BigDecType -> ((l: Number, r: Number) => boolean(toBigDecimal(l) != toBigDecimal(r))),
    ComplexBigIntType -> ((l: Number, r: Number) => boolean(toComplexDouble(l) != toComplexDouble(r))),
    ComplexRationalType -> ((l: Number, r: Number) => boolean(toComplexDouble(l) != toComplexDouble(r))),
    ComplexDoubleType -> ((l: Number, r: Number) => boolean(toComplexDouble(l) != toComplexDouble(r))),
    ComplexBigDecType -> ((l: Number, r: Number) => boolean(toComplexBigDecimal(l) != toComplexBigDecimal(r))),
    QuaternionBigIntType -> ((l: Number, r: Number) => boolean(toQuaternionBigInt(l) == toQuaternionBigInt(r))),
    QuaternionRationalType -> ((l: Number, r: Number) => boolean(toQuaternionRational(l) == toQuaternionRational(r))),
    QuaternionDoubleType -> ((l: Number, r: Number) => boolean(toQuaternionDouble(l) == toQuaternionDouble(r))),
    QuaternionBigDecType -> ((l: Number, r: Number) => boolean(toQuaternionBigDecimal(l) == toQuaternionBigDecimal(r)))
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
    // complex int types
  )

  operation(
    Symbol("\\"),
    IntType -> ((l, r) => (IntType, l.intValue / r.intValue: Number)),
    LongType -> ((l: Number, r: Number) => (LongType, l.longValue / r.longValue: Number)),
    BigIntType -> ((l: Number, r: Number) => (BigIntType, toBigInt(l) / toBigInt(r))),
    // complex int types
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

}
