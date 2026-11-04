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

  describe("RectilinearPolygon") {
    // Simple square: (0,0) -> (4,0) -> (4,4) -> (0,4) -> back to (0,0)
    val square = RectilinearPolygon(Seq(Point(0, 0), Point(4, 0), Point(4, 4), Point(0, 4)))

    describe("contains") {
      it("should return true for a point inside a square") {
        square.contains(Point(2, 2)) shouldBe true
        square.contains(Point(1, 1)) shouldBe true
        square.contains(Point(3, 3)) shouldBe true
      }

      it("should return false for a point outside a square") {
        square.contains(Point(5, 2)) shouldBe false
        square.contains(Point(-1, 2)) shouldBe false
        square.contains(Point(2, 5)) shouldBe false
        square.contains(Point(2, -1)) shouldBe false
      }

      it("should return true for a point on a vertical edge") {
        square.contains(Point(0, 2)) shouldBe true
        square.contains(Point(4, 2)) shouldBe true
      }

      it("should return true for a point on a horizontal edge") {
        square.contains(Point(2, 0)) shouldBe true
        square.contains(Point(2, 4)) shouldBe true
      }

      it("should return true for a corner point") {
        square.contains(Point(0, 0)) shouldBe true
        square.contains(Point(4, 4)) shouldBe true
      }
    }

    describe("contains with complex shape") {
      // U-shaped polygon where a horizontal ray can cross many vertical edges
      //    ___     ___
      //   |   |   |   |
      //   |   |___|   |
      //   |           |
      //   |___________|
      //
      // Points: (0,0) -> (6,0) -> (6,4) -> (4,4) -> (4,2) -> (2,2) -> (2,4) -> (0,4)
      val uShape = RectilinearPolygon(Seq(
        Point(0, 0), Point(6, 0), Point(6, 4), Point(4, 4),
        Point(4, 2), Point(2, 2), Point(2, 4), Point(0, 4)
      ))

      it("should return true for point in left arm of U") {
        uShape.contains(Point(1, 3)) shouldBe true
      }

      it("should return true for point in right arm of U") {
        uShape.contains(Point(5, 3)) shouldBe true
      }

      it("should return true for point in bottom of U") {
        uShape.contains(Point(3, 1)) shouldBe true
      }

      it("should return false for point in the gap of U") {
        uShape.contains(Point(3, 3)) shouldBe false
      }

      it("should return false for point outside U entirely") {
        uShape.contains(Point(7, 2)) shouldBe false
        uShape.contains(Point(-1, 2)) shouldBe false
        uShape.contains(Point(3, 5)) shouldBe false
      }
    }
  }

  describe("solvePart1") {
    it("should work on example input") {
      solvePart1(Point.parsePoints(exampleInput)) shouldBe 50
    }
    it("should work on puzzle input") {
      solvePart1(Point.parsePoints(puzzleInput)) shouldBe 4756718172L
    }
  }

  describe("solvePart2") {
    it("should work on example input") {
      solvePart2(Point.parsePoints(exampleInput)) shouldBe 24
    }
    ignore("should work on puzzle input") {
      solvePart2(Point.parsePoints(puzzleInput)) shouldBe -1
    }
  }

}
