package aoc2021

import aoc.util.*
import aoc.UnitTest

class Day12Spec extends UnitTest:

  "Example 1" should "work" in {
    val example = """start-A
                     start-b
                     A-c
                     A-b
                     b-d
                     A-end
                     b-end"""
    val caveSystem = CaveSystem.parse(example)
    caveSystem.solvePart1 shouldEqual 10
    caveSystem.solvePart2 shouldEqual 36
  }

  "Solution to part 1" should "be correct" in {
    day12Part1 shouldEqual 3510
  }

  "Solution to part 2" should "be correct" in {
    day12Part2 shouldEqual 122880
  }
