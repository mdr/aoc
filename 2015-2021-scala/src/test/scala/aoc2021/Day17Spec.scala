package aoc2021

import aoc.util.*
import aoc.UnitTest

class Day17Spec extends UnitTest:

  "Example" should "work" in {
    val targetArea = TargetArea.parse("target area: x=20..30, y=-10..-5")
    targetArea shouldEqual TargetArea(20, 30, -10, -5)
    val probe = Probe.withVelocity(Point(7, 2))
    Probe.withVelocity(Point(7, 2)).reaches(targetArea) shouldBe true
    Probe.withVelocity(Point(6, 3)).reaches(targetArea) shouldBe true
    Probe.withVelocity(Point(9, 0)).reaches(targetArea) shouldBe true
    Probe.withVelocity(Point(17, -4)).reaches(targetArea) shouldBe false
    Probe.withVelocity(Point(6, 9)).reaches(targetArea) shouldBe true
    Probe.withVelocity(Point(6, 9)).highestY(targetArea) shouldBe 45
    solveDay1(targetArea) shouldEqual 45
    solveDay2(targetArea) shouldEqual 112
  }

  "Solution to part 1" should "be correct" in {
    day17Part1 shouldEqual 6786
  }

  "Solution to part 2" should "be correct" in {
    day17Part2 shouldEqual 2313
  }
