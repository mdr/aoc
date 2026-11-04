package aoc2021

import aoc.util.*

val day14PuzzleInput = Day14PuzzleInput.parse(loadString("aoc2021/input-2021-14.txt"))

def day14Part1: Int = day14PuzzleInput.solvePart1

def day14Part2: Long = day14PuzzleInput.solvePart2

object Day14PuzzleInput:

  def parse(s: String): Day14PuzzleInput =
    val Seq(section1, section2) = s.trim.split("\n\n").toSeq
    val template                = PolymerTemplate(section1.trim)
    def parseRule(s: String): (String, String) =
      val Seq(from, to) = s.split(" -> ").toSeq
      from -> to
    val rules = getLines(section2.trim).map(parseRule).toMap
    Day14PuzzleInput(template, rules)

case class Day14PuzzleInput(template: PolymerTemplate, rules: Map[String, String]):

  def solvePart1: Int =
    val charCounts = iterate(template, 10)(_.apply(rules)).elements.counts.values
    charCounts.max - charCounts.min

  def solvePart2: Long =
    val elementCounts = iterate(counts, 40)(_.apply(rules)).elementCounts.counts
    elementCounts.max - elementCounts.min

  def counts = PolymerElementCounts(Bag.of(template.elements.sliding(2).toSeq))

case class PolymerTemplate(elements: String):

  def apply(rules: Map[String, String]): PolymerTemplate =
    PolymerTemplate(elements.sliding(2).flatMap(s => s.head + rules.getOrElse(s, "")).mkString + elements.last)

case class PolymerElementCounts(counts: Bag[String]):

  def apply(rules: Map[String, String]): PolymerElementCounts =
    val countAdjustments =
      counts.bagFlatMap { case (pair, count) =>
        rules.get(pair) match
          case Some(insert) => Bag(pair -> -count, pair(0) + insert -> count, insert + pair(1) -> count)
          case None         => Bag(pair -> count)
      }
    PolymerElementCounts(counts ++ countAdjustments)

  def elementCounts: Bag[String] =
    counts
      .bagFlatMap { case (pair, count) => Bag(pair.take(1) -> count, pair.drop(1) -> count) }
      .transformCounts(n => if n % 2 == 0 then n / 2 else (n + 1) / 2)
