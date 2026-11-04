package aoc2015
 
import aoc.util.*
import aoc.UnitTest

class Day01Spec extends UnitTest:

  "followFloorInstructions" should "work" in {
    followFloorInstructions("(())") shouldEqual 0
    followFloorInstructions("()()") shouldEqual 0
    followFloorInstructions("(((") shouldEqual 3
    followFloorInstructions("(()(()(") shouldEqual 3
    followFloorInstructions(")())())") shouldEqual -3
  }

  "Solution to part 1" should "be correct" in {
    day01Part1 shouldEqual 232
  }

  "firstInstructionPositionThatEntersBasement" should "work" in {
    firstInstructionPositionThatEntersBasement(")") shouldEqual 1
    firstInstructionPositionThatEntersBasement("()())") shouldEqual 5
  }

  "Solution to part 2" should "be correct" in {
    day01Part2 shouldEqual 1783
  }
