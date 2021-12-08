package aoc2021

import aoc.util.*
import aoc.UnitTest

class Day08Spec extends UnitTest:

  "Example" should "work" in {
    val example =
      """be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
         edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
         fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
         fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
         aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
         fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
         dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
         bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
         egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
         gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"""
    val entries = getLines(example).map(EntryParser.apply)
    countEasyDigits(entries) shouldEqual 26
    solve(entries) shouldEqual 61229
  }

  "Solution to part 1" should "be correct" in {
    day08Part1 shouldEqual 445
  }

  "thing" should "work" in {
    val entry   = EntryParser("acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")
    val mapping = Mapping("cfgabde")
    mapping.decode("ab") shouldEqual StandardWiring(1)
    StandardWiringLookup(mapping.decode("acedgfb").toSet) shouldEqual 8
    StandardWiringLookup(mapping.decode("cdfbe").toSet) shouldEqual 5
    val options = "abcdefg".permutations.map(Mapping(_))
    options.find(_.solves(entry.signalPatterns)) shouldEqual Some(mapping)
    solveEntry(entry) shouldEqual 5353
  }

  "Solution to part 2" should "be correct" ignore {

    day08Part2 shouldEqual 1043101
  }

  "thingy2" should "work" in {
    val state = OneToOneSolveState(Map("a" -> Set(1, 2), "b" -> Set(2), "c" -> Set(3), "d" -> Set(2, 3, 4)))
    state.applyAllDifferentConstraint shouldEqual OneToOneSolveState(
      Map("a" -> Set(1), "b" -> Set(2), "c" -> Set(3), "d" -> Set(4))
    )
  }
