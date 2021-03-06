package xyz.hyperreal.funl

import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class FunLPredefTests extends FreeSpec with ScalaCheckPropertyChecks with Matchers {

  "swap" in {
    runCapture(
      """
				|var a = 3
				|var b = 4
				|
				|swap( a, b )
				|write( a, b )
			""".stripMargin
    ) shouldBe "4, 3"
  }

  "min/max" in {
    runCapture(
      """
				|write( min(3, 2, 22/3, 1/2, 1) )
				|write( max(3, 2, 22/3, 1/2, 1) )
					""".stripMargin
    ) shouldBe
      """
				|1/2
				|22/3
			""".stripMargin.trim
  }

}
