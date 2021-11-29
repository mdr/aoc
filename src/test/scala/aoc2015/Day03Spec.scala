package aoc2015

import aoc.util.*
import aoc.UnitTest

class Day03Spec extends UnitTest {

  "countLocationsVisited" should "work" in {
    countLocationsVisited(parseDirections(">")) shouldEqual 2
    countLocationsVisited(parseDirections("^>v<")) shouldEqual 4
    countLocationsVisited(parseDirections("^v^v^v^v^v")) shouldEqual 2
  }

  "Solution to part 1" should "be correct" in {
    day03Part1 shouldEqual 2565
  }

  "countLocationsBySantaAndRoboSanta" should "work" in {
    countLocationsVisitedBySantaAndRoboSanta(parseDirections("^v")) shouldEqual 3
    countLocationsVisitedBySantaAndRoboSanta(parseDirections("^>v<")) shouldEqual 3
    countLocationsVisitedBySantaAndRoboSanta(parseDirections("^v^v^v^v^v")) shouldEqual 11
  }

  "Solution to part 2" should "be correct" in {
    day03Part2 shouldEqual 2639
  }

}
