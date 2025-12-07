import aoc2025.day07.*
import aoc2025.utils.readInput
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Day07Test extends AnyFunSpec with Matchers {

  val exampleInput: String =
    """.......S.......
      |...............
      |.......^.......
      |...............
      |......^.^......
      |...............
      |.....^.^.^.....
      |...............
      |....^.^...^....
      |...............
      |...^.^...^.^...
      |...............
      |..^...^.....^..
      |...............
      |.^.^.^.^.^...^.
      |...............""".stripMargin
  lazy val puzzleInput: String = readInput("day07/input.txt")

  describe("TachyonManifold.parse") {
    it("should parse start position and splitter rows") {
      val manifold = TachyonManifold.parse(exampleInput)
      manifold.start shouldBe 7
      manifold.splitterRows shouldBe Seq(
        Set(7),
        Set(6, 8),
        Set(5, 7, 9),
        Set(4, 6, 10),
        Set(3, 5, 9, 11),
        Set(2, 6, 12),
        Set(1, 3, 5, 7, 9, 13)
      )
    }
  }

  describe("solvePart1") {
    it("should work on example input") {
      solvePart1(TachyonManifold.parse(exampleInput)) shouldBe 21
    }
    it("should work on puzzle input") {
      solvePart1(TachyonManifold.parse(puzzleInput)) shouldBe 1594
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
