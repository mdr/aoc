package aoc2020

import aoc.UnitTest
import aoc.util.*

class Day01Spec extends UnitTest {

  "findEntriesThatSum" should "work for pairs" in {
    val entryGroup = Seq(1721, 979, 366, 299, 675, 1456)
    val answer  = findEntriesThatSum(entryGroup, numberOfEntries = 2, targetSum = 2020).single
    answer shouldEqual EntryGroup(Seq(299, 1721))
    answer.product shouldEqual 514579
  }

  it should "work for triples" in {
    val entryGroup = Seq(1721, 979, 366, 299, 675, 1456)
    val answer  = findEntriesThatSum(entryGroup, numberOfEntries = 3, targetSum = 2020).single
    answer shouldEqual EntryGroup(Seq(366, 675, 979))
    answer.product shouldEqual 241861950
  }

  "Solution to part 1" should "be correct" in {
    day01Part1 shouldEqual 989824
  }

  "Solution to part 2" should "be correct" in {
    day01Part2 shouldEqual 66432240
  }

}
