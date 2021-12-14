package aoc2021

import aoc.util.*
import aoc.UnitTest

class Day13Spec extends UnitTest:

  "Example" should "work" in {
    val example = """6,10
                    |0,14
                    |9,10
                    |0,3
                    |10,4
                    |4,11
                    |6,0
                    |6,12
                    |4,1
                    |0,13
                    |10,12
                    |3,4
                    |3,0
                    |8,4
                    |1,10
                    |2,14
                    |8,10
                    |9,0
                    |
                    |fold along y=7
                    |fold along x=5""".stripMargin
    val input = PuzzleInput.parse(example)
    input.solvePart1 shouldEqual 17
  }

  "Solution to part 1" should "be correct" in {
    day13Part1 shouldEqual 666
  }

  "Solution to part 2" should "be correct" in {
    puzzleInput.solvePart2 shouldEqual
      """.##....##.#..#..##..####.#..#.#..#.#..#
        |#..#....#.#..#.#..#....#.#..#.#.#..#..#
        |#.......#.####.#..#...#..####.##...#..#
        |#.......#.#..#.####..#...#..#.#.#..#..#
        |#..#.#..#.#..#.#..#.#....#..#.#.#..#..#
        |.##...##..#..#.#..#.####.#..#.#..#..##.""".stripMargin
  }
