package aoc2021

import aoc.util.*
import aoc.UnitTest

class Day11Spec extends UnitTest:

  "Example" should "work" in {
    val example  = """11111
                     19991
                     19191
                     19991
                     11111"""
    val octoGrid = OctoGrid.parse(example)

    octoGrid.step shouldEqual
      OctoGrid
        .parse(
          """34543
             40004
             50005
             40004
             34543"""
        )
        .copy(totalFlashes = 9, stepsTaken = 1)

    octoGrid.step.step shouldEqual
      OctoGrid
        .parse(
          """45654
             51115
             61116
             51115
             45654"""
        )
        .copy(totalFlashes = 9, stepsTaken = 2)
  }

  "Another example" should "work" in {
    val example =
      """5483143223
         2745854711
         5264556173
         6141336146
         6357385478
         4167524645
         2176841721
         6882881134
         4846848554
         5283751526"""
    val octoGrid = OctoGrid.parse(example)
    iterate(octoGrid, 10)(_.step).totalFlashes shouldEqual 204
    solveDay1(octoGrid) shouldEqual 1656
    solveDay2(octoGrid) shouldEqual 195
  }

  "Solution to part 1" should "be correct" in {
    day11Part1 shouldEqual 1773
  }

  "Solution to part 2" should "be correct" in {
    day11Part2 shouldEqual 494
  }
