package xyz.hyperreal.numbers

import math._

abstract class AbstractComplex[T: Numeric, C <: Complex[T, Double, C, ComplexDouble]] extends Complex[T, Double, C, ComplexDouble] {

  protected def promote(re: Double, im: Double) = ComplexDouble(re, im)

  protected def _floor(a: Double) = math.floor(a)

  protected def _ceil(a: Double) = math.ceil(a)

  protected def _sqrt(a: Double) = math.sqrt(a)

  protected def _atan2(y: Double, x: Double) = atan2(y, x)

  protected def _ln(a: Double) = log(a)

  protected def _exp(a: Double) = math.exp(a)

  protected def _sin(a: Double) = math.sin(a)

  protected def _cos(a: Double) = math.cos(a)

  protected def _pow(a: Double, b: Double) = math.pow(a, b)

}
