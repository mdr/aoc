package aoc2021

import aoc.util.*
import aoc.UnitTest

class Day02Spec extends UnitTest:

  "example" should "work" in {
    val example = 
      """forward 5
         down 5
         forward 8
         up 3
         down 8
         forward 2"""
    val instructions = getLines(example).map(SubInstructionParser.apply)
    calculatePart1(instructions) shouldEqual 150
    calculatePart2(instructions) shouldEqual 900
  }

  "Solution to part 1" should "be correct" in {
    day02Part1 shouldEqual 1635930
  }

  "Solution to part 2" should "be correct" in {
    day02Part2 shouldEqual 1781819478
  }
