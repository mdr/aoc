package aoc2021

import aoc.util.*
import aoc.UnitTest

class Day09Spec extends UnitTest:

  "Example" should "work" in {
    val example = """2199943210
                     3987894921
                     9856789892
                     8767896789
                     9899965678"""
    val heightMap = HeightMap.parse(example)
    heightMap.solvePart1 shouldEqual 15
    heightMap.solvePart2 shouldEqual 1134
  }

  "Solution to part 1" should "be correct" in {
    day09Part1 shouldEqual 494
  }

  "Solution to part 2" should "be correct" in {
    day09Part2 shouldEqual 1048128
  }
