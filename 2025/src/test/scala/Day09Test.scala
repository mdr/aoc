import aoc2025.day09.*
import aoc2025.utils.readInput
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Day09Test extends AnyFunSpec with Matchers {

  val exampleInput: String =
    """7,1
      |11,1
      |11,7
      |9,7
      |9,5
      |2,5
      |2,3
      |7,3""".stripMargin
  lazy val puzzleInput: String = readInput("day09/input.txt")

  describe("Point") {
    describe("parsePoints") {
      it("should parse example input") {
        val points = Point.parsePoints(exampleInput)
        points shouldBe Seq(
          Point(7, 1),
          Point(11, 1),
          Point(11, 7),
          Point(9, 7),
          Point(9, 5),
          Point(2, 5),
          Point(2, 3),
          Point(7, 3)
        )
      }
    }
  }

  describe("solvePart1") {
    it("should work on example input") {
      solvePart1(exampleInput) shouldBe 50
    }
    it("should work on puzzle input") {
      solvePart1(puzzleInput) shouldBe 4756718172L
    }
  }

  describe("solvePart2") {
    ignore("should work on example input") {
      solvePart2(exampleInput) shouldBe -1
    }
    ignore("should work on puzzle input") {
      solvePart2(puzzleInput) shouldBe -1
    }
  }

}
