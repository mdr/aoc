package aoc2015

import aoc.util.*
import aoc.UnitTest

class Day02Spec extends UnitTest:

  "wrappingPaperRequired" should "work" in {
    parseBox("2x3x4").wrappingPaperRequired shouldEqual 58
    parseBox("1x1x10").wrappingPaperRequired shouldEqual 43
  }

  "Solution to part 1" should "be correct" in {
    day02Part1 shouldEqual 1586300
  }

  "ribbonRequired" should "work" in {
    parseBox("2x3x4").ribbonRequired shouldEqual 34
    parseBox("1x1x10").ribbonRequired shouldEqual 14
  }

  "Solution to part 2" should "be correct" in {
    day02Part2 shouldEqual 3737498
  }
