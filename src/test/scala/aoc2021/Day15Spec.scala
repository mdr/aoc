package aoc2021

import aoc.util.*
import aoc.UnitTest

class Day15Spec extends UnitTest:

  "Example" should "work" in {
    val example = """1163751742
                    |1381373672
                    |2136511328
                    |3694931569
                    |7463417111
                    |1319128137
                    |1359912421
                    |3125421639
                    |1293138521
                    |2311944581""".stripMargin
    val riskMap = RiskMap.parse(example)
    riskMap.solveDay1 shouldEqual 40
    riskMap.solveDay2 shouldEqual 315
  }

  "Solution to part 1" should "be correct" in {
    day15Part1 shouldEqual 656
  }

  "Solution to part 2" should "be correct" in {
    day15Part2 shouldEqual 2979
  }
