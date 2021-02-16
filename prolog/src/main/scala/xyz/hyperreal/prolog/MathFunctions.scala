package xyz.hyperreal.prolog

import xyz.hyperreal.dal.PrecisionDAL

object MathFunctions {

  def abs(a: Number): Number = PrecisionDAL.absFunction(a)

  def sqrt(a: Number): Number = PrecisionDAL.sqrtFunction(a)

  def acos(a: Number): Number = PrecisionDAL.acosFunction(a)

  def asin(a: Number): Number = PrecisionDAL.asinFunction(a)

//  def atan( a: Number ) = BasicDAL.atanFunction( a )

  def ceiling(a: Number): Number = PrecisionDAL.ceilFunction(a)

  def cos(a: Number): Number = PrecisionDAL.cosFunction(a)

  def sin(a: Number): Number = PrecisionDAL.sinFunction(a)

//  def tan( a: Number ) = BasicDAL.tanFunction( a )

  def exp(a: Number): Number = PrecisionDAL.expFunction(a)

  def floor(a: Number): Number = PrecisionDAL.floorFunction(a)

  def float(a: Number): Number =
    a match {
      case _: java.lang.Double | _: BigDecimal => a
      case _: Integer | _: BigInt              => a.doubleValue
    }

}
