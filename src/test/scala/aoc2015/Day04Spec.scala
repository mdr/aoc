package aoc2015

import aoc.util.*
import aoc.UnitTest

class Day04Spec extends UnitTest:

  "md5" should "work" in {
    toHex(md5("abcdef609043")) should startWith("000001dbbfa")
  }

  "startsWithFiveZeroes" should "work" in {
    startsWithFiveZeroes(md5("abcdef609043")) shouldBe true
  }

  "mine" should "work" in {
    mine("abcdef", startsWithFiveZeroes) shouldEqual 609043
    mine("pqrstuv", startsWithFiveZeroes) shouldEqual 1048970
  }

  "Solution to part 1" should "be correct" in {
    day04Part1 shouldEqual 254575
  }

  "Solution to part 2" should "be correct" in {
    day04Part2 shouldEqual 1038736
  }
