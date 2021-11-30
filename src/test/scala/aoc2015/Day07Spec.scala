package aoc2015

import aoc.util.*
import aoc.UnitTest
import scala.jdk.StreamConverters._

class Day07Spec extends UnitTest:

  "Example" should "be correct" in {
    val example  = """123 -> x
                     456 -> y
                     x AND y -> d
                     x OR y -> e
                     x LSHIFT 2 -> f
                     y RSHIFT 2 -> g
                     NOT x -> h
                     NOT y -> i"""
    val partsAndWires    = organiseParts(example.lines.toScala(Seq).map(PartParser(_)))
    val emulator = new CircuitEmulator(partsAndWires)
    emulator.signals shouldEqual Map(
      "d" -> 72,
      "e" -> 507,
      "f" -> 492,
      "g" -> 114,
      "h" -> 65412,
      "i" -> 65079,
      "x" -> 123,
      "y" -> 456
    )
  }

  "Solution to part 1" should "be correct" in {
    day07Part1 shouldEqual 3176
  }

  "Solution to part 2" should "be correct" in {
    day07Part2 shouldEqual 14710
  }
