package aoc2021

import aoc.util.*
import scala.annotation.tailrec
import de.sciss.fingertree._
import java.util.Date

val riskMap = RiskMap.parse(loadString("aoc2021/input-2021-15.txt"))

def day15Part1 = riskMap.solveDay1

def day15Part2 = riskMap.solveDay2

object RiskMap:
  def parse(s: String) = RiskMap(Grid.parseNumberGrid(s))

case class RiskMap(riskLevels: Grid[Int]):
  def topLeft     = Point(0, 0)
  def bottomRight = Point(riskLevels.width - 1, riskLevels.height - 1)

  def solveDay1 =
    val context =
      DijkstraContext[Point](
        topLeft,
        bottomRight,
        riskLevels.points,
        point => riskLevels.orthogonalNeighbours(point).map(neighbour => neighbour -> riskLevels.valueAt(neighbour))
      )
    context.run

  def solveDay2 = replicate5.solveDay1

  private def wrapRisk(n: Int): Int = (n - 1) % 9 + 1

  def replicate5: RiskMap =
    val newPoints =
      for
        xReplica           <- 0 until 5
        yReplica           <- 0 until 5
        (point, riskLevel) <- riskLevels.values
        newX         = xReplica * riskLevels.width + point.x
        newY         = yReplica * riskLevels.height + point.y
        newPoint     = Point(newX, newY)
        newRiskLevel = wrapRisk(riskLevel + xReplica + yReplica)
      yield newPoint -> newRiskLevel
    RiskMap(Grid(newPoints.toMap, riskLevels.width * 5, riskLevels.height * 5))
