package core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RunnerSpec extends AnyFlatSpec with Matchers {
  "The Runner object" should "print AOC 2022" in {

    val out = new java.io.ByteArrayOutputStream()
    Console.withOut(out) {
      Runner.main(Array[String]())
    }
    out.toString().strip() should include("AOC 2022")
  }
}
