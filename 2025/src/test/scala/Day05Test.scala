import aoc2025.day05.*
import aoc2025.utils.readInput
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Day05Test extends AnyFunSpec with Matchers {

  val exampleInput: String =
    """3-5
      |10-14
      |16-20
      |12-18
      |
      |1
      |5
      |8
      |11
      |17
      |32""".stripMargin
  lazy val puzzleInput: String = readInput("day05/input.txt")

  describe("PuzzleInput.parse") {
    it("should parse ranges and ingredients") {
      val parsed = PuzzleInput.parse(exampleInput)
      parsed.ranges shouldBe Seq(
        FreshRange(3, 5),
        FreshRange(10, 14),
        FreshRange(16, 20),
        FreshRange(12, 18)
      )
      parsed.ingredients shouldBe Seq(1, 5, 8, 11, 17, 32)
    }
  }

  describe("solvePart1") {
    it("should work on example input") {
      solvePart1(PuzzleInput.parse(exampleInput)) shouldBe 3
    }
    it("should work on puzzle input") {
      solvePart1(PuzzleInput.parse(puzzleInput)) shouldBe 525
    }
  }

  describe("solvePart2") {
    it("should work on example input") {
      solvePart2(PuzzleInput.parse(exampleInput)) shouldBe 14
    }
    it("should work on puzzle input") {
      solvePart2(PuzzleInput.parse(puzzleInput)) shouldBe 333892124923577L
    }
  }

}
