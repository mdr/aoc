import aoc2025.day08.*
import aoc2025.utils.readInput
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Day08Test extends AnyFunSpec with Matchers {

  val exampleInput: String =
    """162,817,812
      |57,618,57
      |906,360,560
      |592,479,940
      |352,342,300
      |466,668,158
      |542,29,236
      |431,825,988
      |739,650,466
      |52,470,668
      |216,146,977
      |819,987,18
      |117,168,530
      |805,96,715
      |346,949,466
      |970,615,88
      |941,993,340
      |862,61,35
      |984,92,344
      |425,690,689""".stripMargin
  lazy val puzzleInput: String = readInput("day08/input.txt")

  describe("Point3D") {
    describe("parsePoints") {
      it("should parse input into sequence of points") {
        val input = """1,2,3
                      |4,5,6""".stripMargin
        Point3D.parsePoints(input) shouldBe Seq(Point3D(1, 2, 3), Point3D(4, 5, 6))
      }

      it("should parse first few points of example input") {
        val points = Point3D.parsePoints(exampleInput)
        points.head shouldBe Point3D(162, 817, 812)
        points(1) shouldBe Point3D(57, 618, 57)
        points.size shouldBe 20
      }
    }

    describe("distanceTo") {
      it("should return 0 for same point") {
        val p = Point3D(1, 2, 3)
        p.distanceTo(p) shouldBe 0.0
      }

      it("should calculate distance along single axis") {
        Point3D(0, 0, 0).distanceTo(Point3D(3, 0, 0)) shouldBe 3.0
        Point3D(0, 0, 0).distanceTo(Point3D(0, 4, 0)) shouldBe 4.0
        Point3D(0, 0, 0).distanceTo(Point3D(0, 0, 5)) shouldBe 5.0
      }

      it("should calculate 3D distance") {
        Point3D(0, 0, 0).distanceTo(Point3D(1, 2, 2)) shouldBe 3.0
      }
    }

    describe("ordering") {
      it("should sort by x first") {
        val points = Seq(Point3D(2, 0, 0), Point3D(1, 0, 0), Point3D(3, 0, 0))
        points.sorted shouldBe Seq(Point3D(1, 0, 0), Point3D(2, 0, 0), Point3D(3, 0, 0))
      }

      it("should sort by y when x is equal") {
        val points = Seq(Point3D(1, 3, 0), Point3D(1, 1, 0), Point3D(1, 2, 0))
        points.sorted shouldBe Seq(Point3D(1, 1, 0), Point3D(1, 2, 0), Point3D(1, 3, 0))
      }

      it("should sort by z when x and y are equal") {
        val points = Seq(Point3D(1, 1, 3), Point3D(1, 1, 1), Point3D(1, 1, 2))
        points.sorted shouldBe Seq(Point3D(1, 1, 1), Point3D(1, 1, 2), Point3D(1, 1, 3))
      }

      it("should sort by x, then y, then z") {
        val points = Seq(
          Point3D(2, 1, 1),
          Point3D(1, 2, 1),
          Point3D(1, 1, 2),
          Point3D(1, 1, 1)
        )
        points.sorted shouldBe Seq(
          Point3D(1, 1, 1),
          Point3D(1, 1, 2),
          Point3D(1, 2, 1),
          Point3D(2, 1, 1)
        )
      }
    }
  }

  describe("findClosestPairs") {
    it("should return pairs sorted by distance on example input") {
      val points = Point3D.parsePoints(exampleInput).toSet
      val pairs = Point3D.findClosestPairs(points)

      pairs(0) shouldBe (Point3D(162, 817, 812), Point3D(425, 690, 689))
      pairs(1) shouldBe (Point3D(162, 817, 812), Point3D(431, 825, 988))
      pairs(2) shouldBe (Point3D(805, 96, 715), Point3D(906, 360, 560))
      pairs(3) shouldBe (Point3D(425, 690, 689), Point3D(431, 825, 988))
    }
  }

  describe("solvePart1") {
    it("should work on example input") {
      solvePart1(Point3D.parsePoints(exampleInput).toSet, numberOfBoxesToConnect = 10) shouldBe 40
    }
    it("should work on puzzle input") {
      solvePart1(Point3D.parsePoints(puzzleInput).toSet, numberOfBoxesToConnect = 1000) shouldBe 42315
    }
  }

  describe("solvePart2") {
    it("should work on example input") {
      solvePart2(Point3D.parsePoints(exampleInput).toSet) shouldBe 25272
    }
    it("should work on puzzle input") {
      solvePart2(Point3D.parsePoints(puzzleInput).toSet) shouldBe 8079278220L
    }
  }

}
