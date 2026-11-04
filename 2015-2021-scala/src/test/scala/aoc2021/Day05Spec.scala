package aoc2021

import aoc.util.*
import aoc.UnitTest

class Day05Spec extends UnitTest:

  val example = """0,9 -> 5,9
                  |8,0 -> 0,8
                  |9,4 -> 3,4
                  |2,2 -> 2,1
                  |7,0 -> 7,4
                  |6,4 -> 2,0
                  |0,9 -> 2,9
                  |3,4 -> 1,4
                  |0,0 -> 8,8
                  |5,5 -> 8,2""".stripMargin

  "Example" should "work" in {
    val lines = getLines(example).map(LineSegment.parse)
    lines.length shouldEqual 10
    calculatePart1(lines) shouldEqual 5
    LineSegment.parse("1,1 -> 3,3").points shouldEqual List(Point(1, 1), Point(2, 2), Point(3, 3))
    LineSegment.parse("9,7 -> 7,9").points shouldEqual List(Point(9, 7), Point(8, 8), Point(7, 9))
    calculatePart2(lines) shouldEqual 12
  }

  "Solution to part 1" should "be correct" in {
    day05Part1 shouldEqual 8060
  }

  "Solution to part 2" should "be correct" in {
    day05Part2 shouldEqual 21577
  }
