package aoc2021

import aoc.util.*
import aoc.UnitTest

class Day06Spec extends UnitTest:

  "Example" should "work" in {
    val fishState = SlowFishState.parse("3,4,3,1,2")
    fishState.evolve shouldEqual SlowFishState.parse("2,3,2,0,1")
    fishState.evolve.evolve shouldEqual SlowFishState.parse("1,2,1,6,0,8")
    iterate(fishState, 18)(_.evolve).size shouldEqual 26
    solvePart1(fishState) shouldEqual 5934
  }

  "Solution to part 1" should "be correct" in {
    day06Part1 shouldEqual 380243
  }

  "Example" should "work with method 2" in {
    val fishState = FastFishState.parse("3,4,3,1,2")
    fishState.evolve shouldEqual FastFishState.parse("2,3,2,0,1")
    fishState.evolve.size shouldEqual 5
    fishState.evolve.evolve shouldEqual FastFishState.parse("1,2,1,6,0,8")
    iterate(fishState, 3)(_.evolve) shouldEqual FastFishState.parse("0,1,0,5,6,7,8")
    iterate(fishState, 4)(_.evolve) shouldEqual FastFishState.parse("6,0,6,4,5,6,7,8,8")
    iterate(fishState, 18)(_.evolve).size shouldEqual 26
    solvePart2(fishState) shouldEqual 26984457539L
  }

  "Solution to part 2" should "be correct" in {
    day06Part2 shouldEqual 1708791884591L
  }
