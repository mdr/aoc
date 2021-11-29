package aoc.util

import aoc.util.*
import aoc.UnitTest

class UtilSpec extends UnitTest {

  "uninterleave" should "work" in {
    uninterleave(Seq(1, 2, 3)) shouldEqual (Seq(1, 3), Seq(2))
    uninterleave(Seq(1, 2, 3, 4)) shouldEqual (Seq(1, 3), Seq(2, 4))
    uninterleave(Seq(1)) shouldEqual (Seq(1), Seq.empty)
    uninterleave(Seq.empty) shouldEqual (Seq.empty, Seq.empty)
  } 

}