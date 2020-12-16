package xyz.hyperreal.funl

import xyz.hyperreal.dal.BasicDAL
import xyz.hyperreal.bvm.VM

object Natives {

  def toInt(vm: VM, v: String): Int = v.toInt

  def toLong(vm: VM, v: String): Long = v.toLong

  def toBigInt(vm: VM, v: String) = BigInt(v)

  def toFloat(vm: VM, v: String): Double = v.toDouble

  def eval(vm: VM, expr: String): Any = xyz.hyperreal.funl.run(expr)

  def sqrt(vm: VM, n: Number): Number = BasicDAL.sqrtFunction(n)

  def abs(vm: VM, n: Number): Number = BasicDAL.absFunction(n)

  def exp(vm: VM, n: Number): Number = BasicDAL.expFunction(n)

  def ln(vm: VM, n: Number): Number = BasicDAL.lnFunction(n)

  def sin(vm: VM, n: Number): Number = BasicDAL.sinFunction(n)

  def cos(vm: VM, n: Number): Number = BasicDAL.cosFunction(n)

  def tan(vm: VM, n: Number): Number = BasicDAL.tanFunction(n)
}
