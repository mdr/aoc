package aoc2021

import aoc.util.*
import aoc.UnitTest
import scala.jdk.StreamConverters._

class Day01Spec extends UnitTest:

  "Example" should "work" in {
    val depths = getIntLines(
      """199
         200
         208
         210
         200
         207
         240
         269
         260
         263""")
    countIncreases(depths) shouldEqual 7
    countSlidingWindowIncreases(depths) shouldEqual 5
  }

  "Solution to part 1" should "be correct" in {
    day01Part1 shouldEqual 1616
  }

  "Solution to part 2" should "be correct" in {
    day01Part2 shouldEqual 1645
  }
