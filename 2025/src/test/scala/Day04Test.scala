import aoc2025.day04.*
import aoc2025.utils.readInput
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Day04Test extends AnyFunSpec with Matchers {

  describe("Point") {
    it("should add a delta tuple") {
      Point(2, 3) + (1, -1) shouldBe Point(3, 2)
    }

    it("should return 8 adjacent points") {
      Point(5, 5).adjacent8 shouldBe Set(
        Point(4, 4), Point(4, 5), Point(4, 6),
        Point(5, 4),             Point(5, 6),
        Point(6, 4), Point(6, 5), Point(6, 6)
      )
    }
  }

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
      solvePart1(puzzleGrid) shouldBe 1397
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
