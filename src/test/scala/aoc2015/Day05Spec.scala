package aoc2015

import aoc.util.*
import aoc.UnitTest

class Day05Spec extends UnitTest {

  "containsAtLeastThreeVowels" should "work" in {
    containsAtLeastThreeVowels("ugknbfddgicrmopn") shouldBe true
  }

  "letterAppearsTwiceInARow" should "work" in {
    letterAppearsTwiceInARow("ugknbfddgicrmopn") shouldBe true
  }

  "isNice" should "work" in {
    isNice("ugknbfddgicrmopn") shouldBe true
    isNice("aaa") shouldBe true
    isNice("jchzalrnumimnmhp") shouldBe false
    isNice("haegwjzuvuyypxyu") shouldBe false
    isNice("dvszwmarrgswjxmb") shouldBe false
  }

  "containsPairTwice" should "work" in {
    containsPairTwice("xyxy") shouldBe true
    containsPairTwice("aabcdefgaa") shouldBe true
    containsPairTwice("aaa") shouldBe false
  }

  "containsRepeatedLetterWithLetterBetween" should "work" in {
    containsRepeatedLetterWithLetterBetween("xyx") shouldBe true
    containsRepeatedLetterWithLetterBetween("abcdefeghi") shouldBe true
    containsRepeatedLetterWithLetterBetween("aaa") shouldBe true
  }

  "isNice2" should "work" in {
    isNice2("qjhvhtzxzqqjkmpb") shouldBe true
    isNice2("xxyxx") shouldBe true
    isNice2("uurcxstgmygtbstg") shouldBe false
    isNice2("ieodomkazucvgmuy") shouldBe false
  }

  "Solution to part 1" should "be correct" in {
    day05Part1 shouldEqual 258
  }

  "Solution to part 2" should "be correct" in {
    day05Part2 shouldEqual 53
  }

}
