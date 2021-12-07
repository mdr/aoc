package aoc2021

import aoc.util.*
import aoc.UnitTest

class Day07Spec extends UnitTest:

  "Example" should "work" in {
    val crabs = Crabs.parse("16,1,2,0,4,2,7,1,2,14")
    crabs.optimise(FuelCost.Constant) shouldEqual PositionAndFuel(2, 37)
    crabs.optimise(FuelCost.Increasing) shouldEqual PositionAndFuel(5, 168)
  }

  "Solution to part 1" should "be correct" in {
    day07Part1 shouldEqual 340052
  }

  "Solution to part 2" should "be correct" in {
    day07Part2 shouldEqual 92948968
  }
