package xyz.hyperreal.matcher

import java.io.PrintWriter

import scala.util.Using

object TestGenHaskellMain { //extends App {
  file("basic3")
  file("class")
  file("forced-assoc")
  file("left-assoc")
  file("nullsub3")
  file("osx-bsd-critical")
  file("repetition2")
  file("right-assoc")
  file("totest")

  def file(f: String): Unit = {
    val test = f.replace('-', '_') + "_Tests"
    val out = new PrintWriter("src/test/scala/" + test + ".scala")

    out.println(s"""
				|package xyz.hyperreal.matcher
				|
				|import org.scalatest._
				|import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
				|
				|
				|class $test extends FreeSpec with ScalaCheckPropertyChecks with Matchers with HaskellTest {
			""".stripMargin.trim)

    var prev = ""

    for (line <- Using(io.Source.fromFile(s"regex-posix-unittest-1.1/data-dir/$f.txt"))(_.getLines()).get) {
      out.println()

      line.trim split "\\s+" match {
        case Array(num, regex, str, cap) =>
          out.println("\t\"" + num + "\" in {")

          val r =
            if (regex == "SAME")
              prev
            else {
              prev = regex
              regex
            }

          out.println("\t\ttest( \"\"\"" + r + "\"\"\", \"\"\"" + str + "\"\"\", \"" + cap + "\" ) shouldBe true")
          out.println("\t}")
        case _ => sys.error(s"bad line: $line")
      }
    }

    out.println("}")
    out.close()
  }
}
