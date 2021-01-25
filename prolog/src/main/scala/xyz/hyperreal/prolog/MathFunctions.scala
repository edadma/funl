package xyz.hyperreal.prolog

import xyz.hyperreal.dal.BasicDAL

object MathFunctions {

  def abs(a: Number): Number = BasicDAL.absFunction(a)

  def sqrt(a: Number): Number = BasicDAL.sqrtFunction(a)

  def acos(a: Number): Number = BasicDAL.acosFunction(a)

  def asin(a: Number): Number = BasicDAL.asinFunction(a)

//  def atan( a: Number ) = BasicDAL.atanFunction( a )

  def ceiling(a: Number): Number = BasicDAL.ceilFunction(a)

  def cos(a: Number): Number = BasicDAL.cosFunction(a)

  def sin(a: Number): Number = BasicDAL.sinFunction(a)

//  def tan( a: Number ) = BasicDAL.tanFunction( a )

  def exp(a: Number): Number = BasicDAL.expFunction(a)

  def floor(a: Number): Number = BasicDAL.floorFunction(a)

  def float(a: Number): Number =
    a match {
      case _: java.lang.Double | _: BigDecimal => a
      case _: Integer | _: BigInt              => a.doubleValue
    }

}
