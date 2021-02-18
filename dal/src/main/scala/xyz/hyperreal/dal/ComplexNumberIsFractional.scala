package xyz.hyperreal.dal

trait ComplexNumberIsFractional extends Fractional[Number] {

  def plus(x: Number, y: Number): Number = ComplexDAL.compute(Symbol("+"), x, y)

  def minus(x: Number, y: Number): Number = ComplexDAL.compute(Symbol("-"), x, y)

  def times(x: Number, y: Number): Number = ComplexDAL.compute(Symbol("*"), x, y)

  def div(x: Number, y: Number): Number = ComplexDAL.compute(Symbol("/"), x, y)

  def negate(x: Number): Number = ComplexDAL.negate(x)

  def parseString(str: String): Option[Number] = {
    Some(Integer.decode(str))
  }

  def fromInt(x: Int): Number = x

  def toInt(x: Number): Int = x.intValue

  def toLong(x: Number): Long = x.longValue

  def toFloat(x: Number): Float = x.floatValue

  def toDouble(x: Number): Double = x.doubleValue

  def compare(x: Number, y: Number): Int = ComplexDAL.compare(x, y)

}

object ComplexNumberIsFractional {

  implicit object complexNumberIsFractional extends ComplexNumberIsFractional

}
