package aoc2021

import aoc.util.*
import aoc.UnitTest

class Day14Spec extends UnitTest:

  "Example" should "work" in {
    val example = """NNCB
                    |
                    |CH -> B
                    |HH -> N
                    |CB -> H
                    |NH -> C
                    |HB -> C
                    |HC -> B
                    |HN -> C
                    |NN -> C
                    |BH -> H
                    |NC -> B
                    |NB -> B
                    |BN -> B
                    |BB -> N
                    |BC -> B
                    |CC -> N
                    |CN -> C""".stripMargin
    val input = Day14PuzzleInput.parse(example)
    input.template.apply(input.rules) shouldEqual PolymerTemplate("NCNBCHB")
    input.template.apply(input.rules).apply(input.rules) shouldEqual PolymerTemplate("NBCCNBBBCBHCB")
    iterate(input.template, 10)(_.apply(input.rules)).elements.length shouldEqual 3073
    input.solvePart1 shouldEqual 1588
    input.counts.elementCounts shouldEqual Bag("N" -> 2, "C" -> 1, "B" -> 1)
    input.counts.apply(input.rules).elementCounts shouldEqual Bag("N" -> 2, "C" -> 2, "B" -> 2, "H" -> 1)
    iterate(input.counts, 10)(_.apply(input.rules)).elementCounts("N") shouldEqual 865
    input.solvePart2 shouldEqual 2188189693529L
  }

  "Solution to part 1" should "be correct" in {
    day14Part1 shouldEqual 2712
  }

  "Solution to part 2" should "be correct" in {
    day14Part2 shouldEqual 8336623059567L
  }
