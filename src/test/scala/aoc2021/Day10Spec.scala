package aoc2021

import aoc.util.*
import aoc.UnitTest

class Day10Spec extends UnitTest:

  "Example" should "work" in {
    Line("{([(<{}[<>[]}>{[]{[(<()>").findIllegalCharacter shouldEqual Some('}')
    Line("[[<[([]))<([[{}[[()]]]").findIllegalCharacter shouldEqual Some(')')

    val example =
      """[({(<(())[]>[[{[]{<()<>>
         [(()[<>])]({[<{<<[]>>(
         {([(<{}[<>[]}>{[]{[(<()>
         (((({<>}<{<{<>}{[]{[]{}
         [[<[([]))<([[{}[[()]]]
         [{[{({}]{}}([{[{{{}}([]
         {<[[]]>}<{[{[{[]{()[[[]
         [<(<(<(<{}))><([]([]()
         <{([([[(<>()){}]>(<<{{
         <{([{{}}[<[[[<>{}]]]>[]]"""
    val lines = getLines(example).map(_.trim).map(Line(_))
    solveDay1(lines) shouldEqual 26397

    Line("[({(<(())[]>[[{[]{<()<>>").completeString.mkString shouldEqual "}}]])})]"
    score(Line("[({(<(())[]>[[{[]{<()<>>").completeString) shouldEqual 288957
    solveDay2(lines) shouldEqual 288957
  }

  "Solution to part 1" should "be correct" in {
    day10Part1 shouldEqual 364389
  }

  "Solution to part 2" should "be correct" in {
    day10Part2 shouldEqual 2870201088L
  }
