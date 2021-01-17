//package xyz.hyperreal.numbers
//
//import org.scalatest._
//import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
//
//
//class Tests extends FreeSpec with ScalaCheckPropertyChecks with Matchers {
//
//	"Rational" in
//	{
//		import Rational._
//
//		3\4 shouldBe Rational( 3, 4 )
//		(3\4 match {case Rational( a, b ) => (a, b)}) shouldBe (3, 4)
//		3\4 ^ 5 shouldBe 243\1024
//		3\4 ^ -5 shouldBe 1024\243
//		0\1 ^ 5 shouldBe 0
//		a [RuntimeException] should be thrownBy {0\1 ^ 0}
//		3\4 ^ 1 shouldBe 3\4
//		3\4 ^ 0 shouldBe 1
//		3\4 + 5\6 shouldBe 19\12
//		3\4 - 5\6 shouldBe -1\12
//		3\4 * 5\6 shouldBe 5\8
//		3\4 / 5\6 shouldBe 9\10
//		(5\3).floor shouldBe 1
//		(-5\3).floor shouldBe -2
//		(5\3).ceil shouldBe 2
//		(-5\3).ceil shouldBe -1
//	}
//
//	"ComplexDouble" in
//	{
//
//		import ComplexDouble._
//		import math._
//		import Math._
//
//		(3 + 4*i) + (5 + 6*i) shouldBe 8 + 10*i
//		(3 + 4*i) - (5 + 6*i) shouldBe -2 - 2*i
//		(3 + 4*i).abs shouldBe 5
//		(1.3 + 1.3*i).floor shouldBe 1 + i
//		(1.3 + 1.3*i).ceil shouldBe 2 + 2*i
//		(3 + 4*i)*(5 + 6*i) shouldBe -9 + 38*i
//		(-9 + 38*i)/(5 + 6*i) shouldBe 3 + 4*i
//		(3 + 4*i)*(3 + 4*i).inverse shouldBe 1.0
////		((3 + 4*i) match {case ComplexDouble( a, b ) => (a, b)}) shouldBe (3, 4)
//		(3 + 4*i).sqrt shouldBe 2 + i
// 		(5 + 0*i).sqrt shouldBe sqrt(5)
//		(-1 + 0*i).sqrt shouldBe i
// 		(-4 + 0*i).sqrt shouldBe 2*i
//		(Pi*i).exp + 1 shouldBe 0
//		(1 + Pi*i).exp shouldBe -E
//		(1*i)^(1*i) shouldBe exp(-Pi/2)
//		(3 + 4*i)^0.5 shouldBe (3 + 4*i).sqrt
//		(3 + 4*i)^10 shouldBe -9653287+1476984*i
// 		(3 + 0*i).cos shouldBe cos(3)
// 		(3 + 0*i).cosh shouldBe cosh(3)
// 		(.5 + 0*i).acos shouldBe acos(.5)
// 		(3 + 0*i).acosh shouldBe acosh(3)
// 		(3 + 0*i).sin shouldBe sin(3)
//		(3 + 0*i).sinh shouldBe sinh(3)
//		(.5 + 0*i).asin shouldBe asin(.5)
//		(3 + 0*i).asinh shouldBe asinh(3)
//		(3 + 0*i).tan shouldBe tan(3)
//		(.5 + 0*i).atan shouldBe atan(.5)
//		(.5 + 0*i).atanh shouldBe atanh(.5)
//		(3 + 0*i).tanh shouldBe tanh(3)
// 		assert( (3 + 4*i).asin.sin roughly (3 + 4*i) )
//		assert( (3 + 4*i).acos.cos roughly (3 + 4*i) )
//		assert( (3 + 4*i).atan.tan roughly (3 + 4*i) )
//		assert( (3 + 4*i).asinh.sinh roughly (3 + 4*i) )
//		assert( (3 + 4*i).acosh.cosh roughly (3 + 4*i) )
//		assert( (3 + 4*i).atanh.tanh roughly (3 + 4*i) )
//	}
//
//	"ComplexRational" in
//	{
//
//		import ComplexRational._
//		import Rational._
//		import math._
//		import Math._
//
//		(3 + 4*i) + (5 + 6*i) shouldBe 8 + 10*i
//		(3 + 4*i) - (5 + 6*i) shouldBe -2 - 2*i
//		(3 + 4*i).abs shouldBe 5
////		(5\3 + 7\3*i).floor shouldBe 1 + 2*i//todo: correct
////		(5\3 + 7\3*i).ceil shouldBe 2 + 3*i
//		(3 + 4*i)*(5 + 6*i) shouldBe -9 + 38*i
//		(-9 + 38*i)/(5 + 6*i) shouldBe 3 + 4*i
//		(3 + 4*i)*(3 + 4*i).inverse shouldBe 1
//		//		((3 + 4*i) match {case ComplexDouble( a, b ) => (a, b)}) shouldBe (3, 4)
////		(3 + 4*i).sqrt shouldBe 2 + i//todo: correct
////		(5 + 0*i).sqrt shouldBe sqrt(5)
////		(-1 + 0*i).sqrt shouldBe i
////		(-4 + 0*i).sqrt shouldBe 2*i
////		(Pi*i).exp + 1 shouldBe 0
////		(1 + Pi*i).exp shouldBe -E
//		(1*i)^(1*i) shouldBe exp(-Pi/2)
//		(3 + 4*i)^0.5 shouldBe (3 + 4*i).sqrt
//		(3 + 4*i)^10 shouldBe -9653287+1476984*i
//		(3 + 0*i).cos shouldBe cos(3)
//		(3 + 0*i).cosh shouldBe cosh(3)
////		(.5 + 0*i).acos shouldBe acos(.5)
//		(3 + 0*i).acosh shouldBe acosh(3)
//		(3 + 0*i).sin shouldBe sin(3)
//		(3 + 0*i).sinh shouldBe sinh(3)
////		(.5 + 0*i).asin shouldBe asin(.5)
//		(3 + 0*i).asinh shouldBe asinh(3)
//		(3 + 0*i).tan shouldBe tan(3)
////		(.5 + 0*i).atan shouldBe atan(.5)
////		(.5 + 0*i).atanh shouldBe atanh(.5)
//		(3 + 0*i).tanh shouldBe tanh(3)
////		assert( (3 + 4*i).asin.sin roughly (3 + 4*i) )
////		assert( (3 + 4*i).acos.cos roughly (3 + 4*i) )
////		assert( (3 + 4*i).atan.tan roughly (3 + 4*i) )
////		assert( (3 + 4*i).asinh.sinh roughly (3 + 4*i) )
////		assert( (3 + 4*i).acosh.cosh roughly (3 + 4*i) )
////		assert( (3 + 4*i).atanh.tanh roughly (3 + 4*i) )
//	}
//
//	"BigDecimalMath" in
//	{
//		import math._
//		import BigDecimalMath.decimal128._
//
//		(BigDecimalMath.sqrt( 3 )*BigDecimalMath.sqrt( 3 )).toDouble shouldBe 3
// 		bdmath.Pi.v.toDouble shouldBe Pi
// 		bdmath.E.v.toDouble shouldBe E
// 		BigDecimalMath.exp( 0 ) shouldBe 1
// 		BigDecimalMath.exp( -1 ).toDouble shouldBe 1/E
// 		BigDecimalMath.exp( 1 ).toDouble shouldBe E
// 		BigDecimalMath.exp( 100 ).toDouble shouldBe exp( 100 )
// 		BigDecimalMath.exp(BigDecimalMath.ln( 1.1 )).toDouble shouldBe 1.1
////			nearly( BigDecimalMath.ln( 1 ).toDouble, 0 ) shouldBe true
//		BigDecimalMath.ln( bdmath.E.v ).toDouble shouldBe 1
// 		BigDecimalMath.ln( bdmath.E.v*bdmath.E.v ).toDouble shouldBe 2
// 		BigDecimalMath.ln( BigDecimalMath.exp(5) ).toDouble shouldBe 5
////		BigDecimalMath.sin( 0 ) shouldBe 0
////		bdmath.sin( bdmath.Pi.v/2 ).toDouble shouldBe 1
//// 		nearly( bdmath.sin( bdmath.Pi.v ).toDouble, 0 ) shouldBe true
////			bdmath.sin( 1.5*bdmath.Pi.v ).toDouble shouldBe -1
//// 		nearly( bdmath.sin( 2*bdmath.Pi.v ).toDouble, 0 ) shouldBe true
////			bdmath.cos( 0 ) shouldBe 1
//// 		nearly( bdmath.cos( bdmath.Pi.v/2 ).toDouble, 0 ) shouldBe true
//// 		nearly( bdmath.cos( bdmath.Pi.v ).toDouble, -1 ) shouldBe true
//// 		nearly( bdmath.cos( 1.5*bdmath.Pi.v ).toDouble, 0 ) shouldBe true
//// 		nearly( bdmath.cos( 2*bdmath.Pi.v ).toDouble, 1 ) shouldBe true
//// 		bdmath.acos( 0 ).toDouble shouldBe Pi/2
//// 		bdmath.acos( 1 ).toDouble shouldBe 0
//// 		bdmath.acos( -1 ).toDouble shouldBe Pi
//// 		bdmath.acos( .5 ).toDouble shouldBe acos( .5 )
//// 		bdmath.atan( .5 ).toDouble shouldBe atan( .5 )
//// 		bdmath.atan( 1 ).toDouble shouldBe Pi/4
//// 		bdmath.atan( 0 ).toDouble shouldBe 0
//// 		bdmath.atan( -1 ).toDouble shouldBe -Pi/4
//// 		bdmath.atan2( 0, 0).toDouble shouldBe 0
//// 		bdmath.atan2( 1, 0).toDouble shouldBe Pi/2
//// 		bdmath.atan2( 0, 1).toDouble shouldBe 0
//// 		bdmath.atan2( 1, 1).toDouble shouldBe Pi/4
//// 		bdmath.atan2( -1, 0).toDouble shouldBe -Pi/2
//// 		bdmath.atan2( 0, -1).toDouble shouldBe Pi
//// 		bdmath.atan2( -1, -1).toDouble shouldBe -.75*Pi
//// 		bdmath.atan2( -1, 1).toDouble shouldBe -Pi/4
//// 		bdmath.atan2( 1, -1).toDouble shouldBe .75*Pi
//// 		bdmath.atanh( .5 ).round( MathContext.DECIMAL64 ).toDouble shouldBe Math.atanh( .5 )
//// 		pow( BigInt(3), BigInt(5) ) shouldBe 243
//// 		pow( BigInt(0), BigInt(0) ) shouldBe 0
//// 		pow( BigInt(3), BigInt(1) ) shouldBe 3
//// 		pow( BigInt(3), BigInt(0) ) shouldBe 1
//// 		pow( BigInt(0), BigInt(3) ) shouldBe 0
//	}
//
//}
