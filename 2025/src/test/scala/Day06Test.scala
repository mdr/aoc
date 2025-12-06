import aoc2025.day06.*
import aoc2025.utils.readInput
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Day06Test extends AnyFunSpec with Matchers {

  val exampleInput: String =
    """123 328  51 64
      | 45 64  387 23
      |  6 98  215 314
      |*   +   *   +""".stripMargin
  lazy val puzzleInput: String = readInput("day06/input.txt")

  describe("PuzzleInput.parse") {
    it("should parse number rows and operators") {
      val parsed = PuzzleInput.parse(exampleInput)
      parsed.numberRows shouldBe Seq(
        Seq(123, 328, 51, 64),
        Seq(45, 64, 387, 23),
        Seq(6, 98, 215, 314)
      )
      parsed.operators shouldBe Seq(Operator.Multiply, Operator.Add, Operator.Multiply, Operator.Add)
    }
  }

  describe("solvePart1") {
    it("should work on example input") {
      solvePart1(PuzzleInput.parse(exampleInput)) shouldBe 4277556L
    }
    it("should work on puzzle input") {
      solvePart1(PuzzleInput.parse(puzzleInput)) shouldBe 4722948564882L
    }
  }

  describe("solvePart2") {
    it("should work on example input") {
      pending
      // solvePart2(PuzzleInput.parse(exampleInput)) shouldBe ???
    }
    it("should work on puzzle input") {
      pending
      // solvePart2(PuzzleInput.parse(puzzleInput)) shouldBe ???
    }
  }

}
