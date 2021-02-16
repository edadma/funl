package xyz.hyperreal.dal

import xyz.hyperreal.numbers.{BigDecimalMath, Rational}
import xyz.hyperreal.numbers.BigDecimalMath.decimal128._

import java.{lang => boxed}
import scala.math.{BigInt, pow}

object BasicDAL extends DAL {

  operation(
    Symbol("+"),
    IntType -> ((l: Number, r: Number) => maybePromote(l.longValue + r.longValue)),
    LongType -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) + toBigInt(r))),
    BigIntType -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) + toBigInt(r))),
    DoubleType -> ((l: Number, r: Number) => (DoubleType, l.doubleValue + r.doubleValue: Number)),
  )

  operation(
    Symbol("-"),
    IntType -> ((l: Number, r: Number) => maybePromote(l.longValue - r.longValue)),
    LongType -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) - toBigInt(r))),
    BigIntType -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) - toBigInt(r))),
    DoubleType -> ((l: Number, r: Number) => (DoubleType, l.doubleValue - r.doubleValue: Number)),
  )

  operation(
    Symbol("*"),
    IntType -> ((l: Number, r: Number) => maybePromote(l.longValue * r.longValue)),
    LongType -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) * toBigInt(r))),
    BigIntType -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) * toBigInt(r))),
    DoubleType -> ((l: Number, r: Number) => (DoubleType, l.doubleValue * r.doubleValue: Number)),
  )

  operation(
    Symbol("/"),
    IntType -> ((l: Number, r: Number) => intOrDouble(toRational(l) / toRational(r))),
    LongType -> ((l: Number, r: Number) => intOrDouble(toRational(l) / toRational(r))),
    BigIntType -> ((l: Number, r: Number) => intOrDouble(toRational(l) / toRational(r))),
    DoubleType -> ((l: Number, r: Number) => (DoubleType, l.doubleValue / r.doubleValue: Number)),
  )

  operation(
    Symbol("//"),
    IntType -> ((l: Number, r: Number) => (DoubleType, l.doubleValue / r.doubleValue: Number)),
    LongType -> ((l: Number, r: Number) => (DoubleType, l.doubleValue / r.doubleValue: Number)),
    BigIntType -> ((l: Number, r: Number) => (DoubleType, l.doubleValue / r.doubleValue: Number)),
    DoubleType -> ((l: Number, r: Number) => (DoubleType, l.doubleValue / r.doubleValue: Number)),
  )

  operation(
    Symbol("^"),
    IntType -> ((l: Number, r: Number) => bigIntPow(l, r)),
    LongType -> ((l: Number, r: Number) => bigIntPow(l, r)),
    BigIntType -> ((l: Number, r: Number) => bigIntPow(l, r)),
    DoubleType -> ((l: Number, r: Number) => (DoubleType, pow(l.doubleValue, r.doubleValue): Number)),
  )

  operation(
    Symbol("mod"),
    IntType -> ((l: Number, r: Number) => (IntType, l.intValue % r.intValue: Number)),
    LongType -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) % toBigInt(r))),
    BigIntType -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) % toBigInt(r))),
    DoubleType -> ((l: Number, r: Number) => (DoubleType, l.doubleValue % r.doubleValue: Number)),
  )

  relation(
    Symbol("="),
    IntType -> ((l: Number, r: Number) => boolean(l == r)),
    LongType -> ((l: Number, r: Number) => boolean(l.longValue == r.longValue)),
    BigIntType -> ((l: Number, r: Number) => boolean(toBigInt(l) == toBigInt(r))),
    DoubleType -> ((l: Number, r: Number) => boolean(l.doubleValue == r.doubleValue)),
  )

  relation(
    Symbol("!="),
    IntType -> ((l: Number, r: Number) => boolean(l != r)),
    LongType -> ((l: Number, r: Number) => boolean(l.longValue != r.longValue)),
    BigIntType -> ((l: Number, r: Number) => boolean(toBigInt(l) != toBigInt(r))),
    DoubleType -> ((l: Number, r: Number) => boolean(l.doubleValue != r.doubleValue)),
  )

  relation(
    Symbol("<"),
    IntType -> ((l: Number, r: Number) => boolean(l.intValue < r.intValue)),
    LongType -> ((l: Number, r: Number) => boolean(l.longValue < r.longValue)),
    BigIntType -> ((l: Number, r: Number) => boolean(toBigInt(l) < toBigInt(r))),
    DoubleType -> ((l: Number, r: Number) => boolean(l.doubleValue < r.doubleValue)),
  )

  relation(
    Symbol(">"),
    IntType -> ((l: Number, r: Number) => boolean(l.intValue > r.intValue)),
    LongType -> ((l: Number, r: Number) => boolean(l.longValue > r.longValue)),
    BigIntType -> ((l: Number, r: Number) => boolean(toBigInt(l) > toBigInt(r))),
    DoubleType -> ((l: Number, r: Number) => boolean(l.doubleValue > r.doubleValue)),
  )

  relation(
    Symbol("<="),
    IntType -> ((l: Number, r: Number) => boolean(l.intValue <= r.intValue)),
    LongType -> ((l: Number, r: Number) => boolean(l.longValue <= r.longValue)),
    BigIntType -> ((l: Number, r: Number) => boolean(toBigInt(l) <= toBigInt(r))),
    DoubleType -> ((l: Number, r: Number) => boolean(l.doubleValue <= r.doubleValue)),
  )

  relation(
    Symbol(">="),
    IntType -> ((l: Number, r: Number) => boolean(l.intValue >= r.intValue)),
    LongType -> ((l: Number, r: Number) => boolean(l.longValue >= r.longValue)),
    BigIntType -> ((l: Number, r: Number) => boolean(toBigInt(l) >= toBigInt(r))),
    DoubleType -> ((l: Number, r: Number) => boolean(l.doubleValue >= r.doubleValue)),
  )

  relation(
    Symbol("div"),
    IntType -> ((l, r) => boolean(r.intValue % l.intValue == 0)),
    LongType -> ((l: Number, r: Number) => boolean(r.longValue % l.longValue == 0)),
    BigIntType -> ((l: Number, r: Number) => boolean(toBigInt(r) % toBigInt(l) == 0)),
  )

  operation(
    Symbol("\\"),
    IntType -> ((l, r) => (IntType, l.intValue / r.intValue: Number)),
    LongType -> ((l: Number, r: Number) => (LongType, l.longValue / r.longValue: Number)),
    BigIntType -> ((l: Number, r: Number) => (BigIntType, toBigInt(l) / toBigInt(r))),
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
    DoubleType -> ((l: Number, r: Number) => (IntType, l.doubleValue compare r.doubleValue: Number)),
  )

}
