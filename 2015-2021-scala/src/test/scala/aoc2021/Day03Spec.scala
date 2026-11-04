package aoc2021

import aoc.util.*
import aoc.UnitTest

class Day03Spec extends UnitTest:

  "example" should "work" in {
    val example =
      """00100
         11110
         10110
         10111
         10101
         01111
         00111
         11100
         10000
         11001
         00010
         01010"""
    val numbers = BinaryNumberCollection(getLines(example).map(_.trim).map(BinaryNumber.apply))
    numbers.gammaRate shouldEqual 22
    numbers.epsilonRate shouldEqual 9
    numbers.powerConsumption shouldEqual 198
  
    numbers.oxygenGeneratorRating shouldEqual 23
    numbers.co2ScrubberRating shouldEqual 10
    numbers.lifeSupportRating shouldEqual 230
  }

  "Solution to part 1" should "be correct" in {
    day03Part1 shouldEqual 4191876
  }

  "Solution to part 2" should "be correct" in {
    day03Part2 shouldEqual 3414905
  }
