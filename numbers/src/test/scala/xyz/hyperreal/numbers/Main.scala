package xyz.hyperreal.numbers

//import Rational._
import ComplexBigDecimal._

object Main extends App {

// 	val z = 1\2 + 2\3*i
// 	implicit val bdmath = new BigDecimalMath( 80 )
  import BigDecimalMath.decimal128._

  val a = ComplexBigDecimal(1, 2)
  val z = 1 + 2 * i
  val p = 5 + 6 * i

  println(z.cos.acos)
  println(z.sin.asin)
// 	println( z.tan.atan )
  println(z ^ 0.5, z.sqrt)
  println(z * z, z ^ 2)
// 	println( z^(-2) )
// 	println( (z^2) * (z^(-2)) )

}
