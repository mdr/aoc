package utils

import aoc.util.*
import aoc.UnitTest

class UtilsSpec extends UnitTest:

  "map merge" should "work" in {
    val map1 = Map("a" -> 2, "b" -> 4, "x" -> 1)
    val map2 = Map("b" -> 5, "c" -> 1, "a" -> 1, "y" -> 2)
    map1.mergeWith(map2)(_ + _) shouldEqual
      Map("a" -> 3, "b" -> 9, "c" -> 1, "x" -> 1, "y" -> 2)
  }
