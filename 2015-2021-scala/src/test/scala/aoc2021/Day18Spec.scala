package aoc2021

import aoc.util.*
import aoc.UnitTest

class Day18Spec extends UnitTest:

  "Example" should "work" in {
    SnailfishNumber("[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]").toString shouldEqual
      "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]"
    SnailfishNumber("[[3,4],5]").paths.map(_.toString) shouldEqual Seq("LL", "L", "LR", "", "R")
    SnailfishNumber("[[[[[9,8],1],2],3],4]").leafPaths.map(_.toString) shouldEqual Seq(
      "LLLLL",
      "LLLLR",
      "LLLR",
      "LLR",
      "LR",
      "R"
    )
    val n = SnailfishNumber("[[[[[9,8],1],2],3],4]")
    n.findPairToExplode() shouldEqual Some(ExplodeCandidate(Path("LLLL"), 9, 8))
    val n2 = n.replace(Path("LLLL"), RegularNumber(0))
    n2 shouldEqual SnailfishNumber("[[[[0,1],2],3],4]")
    n2.nextLeafPath(Path("LLLL")) shouldEqual Some(Path("LLLR"))
    n2.previousLeafPath(Path("LLLL")) shouldEqual None
    n.explode(n.findPairToExplode().get) shouldEqual SnailfishNumber("[[[[0,9],2],3],4]")
    SnailfishNumber("[7,[6,[5,[4,[3,2]]]]]").tryExplode shouldEqual Some(SnailfishNumber("[7,[6,[5,[7,0]]]]"))
    SnailfishNumber("[[6,[5,[4,[3,2]]]],1]").tryExplode shouldEqual Some(SnailfishNumber("[[6,[5,[7,0]]],3]"))
    SnailfishNumber("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]").tryExplode shouldEqual Some(
      SnailfishNumber("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
    )
    SnailfishNumber("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]").tryExplode shouldEqual Some(
      SnailfishNumber("[[3,[2,[8,0]]],[9,[5,[7,0]]]]")
    )
    SnailfishNumber("[[[[0,7],4],[15,[0,13]]],[1,1]]").trySplit shouldEqual Some(
      SnailfishNumber("[[[[0,7],4],[[7,8],[0,13]]],[1,1]]")
    )
    SnailfishNumber("[[[[0,7],4],[[7,8],[0,13]]],[1,1]]").trySplit shouldEqual Some(
      SnailfishNumber("[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]")
    )
    SnailfishNumber("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]").reduce shouldEqual SnailfishNumber(
      "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"
    )
    SnailfishNumber("[[[[4,3],4],4],[7,[[8,4],9]]]") + SnailfishNumber("[1,1]") shouldEqual SnailfishNumber(
      "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"
    )

    val example = """[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
                     [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
                     [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
                     [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
                     [7,[5,[[3,8],[1,4]]]]
                     [[2,[2,2]],[8,[8,1]]]
                     [2,9]
                     [1,[[[9,3],9],[[9,0],[0,7]]]]
                     [[[5,[7,4]],7],1]
                     [[[[4,2],2],6],[8,7]]"""
    getLines(example).map(SnailfishNumber.apply).reduce(_ + _) shouldEqual SnailfishNumber("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")

    SnailfishNumber("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]").magnitude shouldEqual 3488

    val example2 = """[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
                      [[[5,[2,8]],4],[5,[[9,9],0]]]
                      [6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
                      [[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
                      [[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
                      [[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
                      [[[[5,4],[7,7]],8],[[8,3],8]]
                      [[9,3],[[9,9],[6,[4,9]]]]
                      [[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
                      [[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"""
    val numbers = getLines(example2).map(SnailfishNumber.apply)
    solveDay18Part1(numbers) shouldEqual 4140
    solveDay18Part2(numbers) shouldEqual 3993
  }

  "Solution to part 1" should "be correct" in {
    day18Part1 shouldEqual 4391
  }

  "Solution to part 2" should "be correct" in {
    day18Part2 shouldEqual 4626
  }
