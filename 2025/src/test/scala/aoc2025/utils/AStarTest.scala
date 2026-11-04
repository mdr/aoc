package aoc2025.utils

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class AStarTest extends AnyFunSpec with Matchers {

  case class Point(x: Int, y: Int)

  describe("AStar.findMinCost") {

    it("should return 0 when start equals goal") {
      val result = AStar.findMinCost[Int](
        start = 5,
        goal = 5,
        neighbors = _ => Seq.empty
      )
      result shouldBe Some(0.0)
    }

    it("should find direct path") {
      val result = AStar.findMinCost[Int](
        start = 0,
        goal = 1,
        neighbors = {
          case 0 => Seq((1, 10.0))
          case _ => Seq.empty
        }
      )
      result shouldBe Some(10.0)
    }

    it("should find shortest path in simple graph") {
      // Graph: 0 -> 1 (cost 1) -> 2 (cost 1)
      //        0 -> 2 (cost 5)
      val result = AStar.findMinCost[Int](
        start = 0,
        goal = 2,
        neighbors = {
          case 0 => Seq((1, 1.0), (2, 5.0))
          case 1 => Seq((2, 1.0))
          case _ => Seq.empty
        }
      )
      result shouldBe Some(2.0)
    }

    it("should return None when no path exists") {
      val result = AStar.findMinCost[Int](
        start = 0,
        goal = 2,
        neighbors = {
          case 0 => Seq((1, 1.0))
          case _ => Seq.empty
        }
      )
      result shouldBe None
    }

    it("should work with heuristic for grid navigation") {
      def manhattanDistance(p: Point, goal: Point): Double =
        (math.abs(goal.x - p.x) + math.abs(goal.y - p.y)).toDouble

      val goal = Point(2, 2)
      val result = AStar.findMinCost[Point](
        start = Point(0, 0),
        goal = goal,
        neighbors = p => Seq(
          Point(p.x + 1, p.y),
          Point(p.x - 1, p.y),
          Point(p.x, p.y + 1),
          Point(p.x, p.y - 1)
        ).map(n => (n, 1.0)),
        heuristic = p => manhattanDistance(p, goal)
      )
      result shouldBe Some(4.0)
    }

    it("should skip nodes with infinite heuristic") {
      // Graph: 0 -> 1 -> 2 (blocked) or 0 -> 3 -> 2
      val blocked = Set(1)
      val result = AStar.findMinCost[Int](
        start = 0,
        goal = 2,
        neighbors = {
          case 0 => Seq((1, 1.0), (3, 2.0))
          case 1 => Seq((2, 1.0))
          case 3 => Seq((2, 1.0))
          case _ => Seq.empty
        },
        heuristic = n => if blocked.contains(n) then Double.PositiveInfinity else 0.0
      )
      result shouldBe Some(3.0) // 0 -> 3 -> 2
    }

    it("should handle cycles correctly") {
      // Graph with cycle: 0 -> 1 -> 2 -> 0, goal is 2
      val result = AStar.findMinCost[Int](
        start = 0,
        goal = 2,
        neighbors = {
          case 0 => Seq((1, 1.0))
          case 1 => Seq((2, 1.0), (0, 1.0))
          case 2 => Seq((0, 1.0))
          case _ => Seq.empty
        }
      )
      result shouldBe Some(2.0)
    }
  }
}
