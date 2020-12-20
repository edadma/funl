///*
//package xyz.hyperreal.dal
//
//trait NumberIsFractional extends Fractional[TypedNumber] {
//
//  def plus(x: TypedNumber, y: TypedNumber): TypedNumber = BasicDAL.compute(Symbol("+"), x, y)
//
//  def minus(x: TypedNumber, y: TypedNumber): TypedNumber = BasicDAL.compute(Symbol("-"), x, y)
//
//  def times(x: TypedNumber, y: TypedNumber): TypedNumber = BasicDAL.compute(Symbol("*"), x, y)
//
//  def div(x: TypedNumber, y: TypedNumber): TypedNumber = BasicDAL.compute(Symbol("/"), x, y)
//
//  def negate(x: TypedNumber): TypedNumber = BasicDAL.negate(x)
//
//  def parseString(str: String): Option[TypedNumber] = {
//    Some(Integer.decode(str))
//  }
//
//  def fromInt(x: Int): TypedNumber = x
//
//  def toInt(x: TypedNumber): Int = x.intValue
//
//  def toLong(x: TypedNumber): Long = x.longValue
//
//  def toFloat(x: TypedNumber): Float = x.floatValue
//
//  def toDouble(x: TypedNumber): Double = x.doubleValue
//
//  def compare(x: TypedNumber, y: TypedNumber): Int = BasicDAL.compare(x, y)
//
//}
//
//object NumberIsFractional {
//
//  implicit object numberIsFractional extends NumberIsFractional
//
//}
//*/
