import aoc2025.day04.*
import aoc2025.utils.readInput
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Day04Test extends AnyFunSpec with Matchers {

  val exampleInput: String =
    """..@@.@@@@.
      |@@@.@.@.@@
      |@@@@@.@.@@
      |@.@@@@..@.
      |@@.@@@@.@@
      |.@@@@@@@.@
      |.@.@.@.@@@
      |@.@@@.@@@@
      |.@@@@@@@@.
      |@.@.@@@.@.""".stripMargin
  val exampleGrid: PaperGrid = PaperGrid.parse(exampleInput)
  val puzzleInput: String = readInput("day04/input.txt")
  val puzzleGrid: PaperGrid = PaperGrid.parse(puzzleInput)

  describe("PaperGrid.parse") {
    it("should parse @ as paper roll positions") {
      val input = """.@.
                    |@.@
                    |..@""".stripMargin
      PaperGrid.parse(input) shouldBe PaperGrid(Set(
        Point(0, 1),
        Point(1, 0), Point(1, 2),
        Point(2, 2)
      ))
    }
  }

  describe("solvePart1") {
    it("should work on example input") {
      solvePart1(exampleGrid) shouldBe 13
    }
    it("should work on puzzle input") {
      pending
      // solvePart1(puzzleGrid) shouldBe ???
    }
  }

  describe("solvePart2") {
    it("should work on example input") {
      solvePart2(exampleGrid) shouldBe 43
    }
    it("should work on puzzle input") {
      solvePart2(puzzleGrid) shouldBe 8758
    }
  }

}
