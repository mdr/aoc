package aoc2025.day05

import aoc2025.utils.{readInput, splitOnBlankLines, sumBy}

type IngredientId = Long

case class FreshRange(start: IngredientId, end: IngredientId):
  def contains(value: IngredientId): Boolean = value >= start && value <= end

  def size: Long = end - start + 1

object FreshRange:
  def parse(input: String): FreshRange =
    val Array(start, end) = input.split("-").map(_.toLong)
    FreshRange(start, end)

case class PuzzleInput(ranges: Seq[FreshRange], ingredients: Seq[IngredientId]):

  private def isFresh(ingredient: IngredientId): Boolean = ranges.exists(_.contains(ingredient))

  def countFreshIngredients: Int = ingredients.count(isFresh)

object PuzzleInput:
  def parse(input: String): PuzzleInput =
    val Seq(rangeSection, ingredientsSection) = input.splitOnBlankLines
    val ranges = rangeSection.linesIterator.map(FreshRange.parse).toSeq
    val ingredients = ingredientsSection.linesIterator.map(_.toLong).toSeq
    PuzzleInput(ranges, ingredients)

case class SortedDisjointRanges(ranges: Seq[FreshRange]):
  assert(ranges.sliding(2).forall {
    case Seq(a, b) => a.end < b.start
    case _ => true
  }, "Ranges must be non-overlapping and sorted")

  def add(range: FreshRange): SortedDisjointRanges = {
    def insertAndMaybeMerge(ranges: Seq[FreshRange], newRange: FreshRange): Seq[FreshRange] = ranges match {
      case Nil => Seq(newRange)
      case head +: tail =>
        if (newRange.end < head.start) {
          newRange +: ranges
        } else if (newRange.start > head.end) {
          head +: insertAndMaybeMerge(tail, newRange)
        } else {
          val mergedRange = FreshRange(
            start = Math.min(head.start, newRange.start),
            end = Math.max(head.end, newRange.end)
          )
          insertAndMaybeMerge(tail, mergedRange)
        }
    }
    SortedDisjointRanges(insertAndMaybeMerge(ranges, range))
  }

  def size: Long = ranges.sumBy(_.size)

object SortedDisjointRanges:
  def empty: SortedDisjointRanges = SortedDisjointRanges(Seq.empty)

def solvePart1(input: PuzzleInput) = input.countFreshIngredients

def solvePart2(input: PuzzleInput) = input.ranges.foldLeft(SortedDisjointRanges.empty)(_.add(_)).size

@main def day05(): Unit =
  val input = readInput("day05/input.txt")
  val puzzleInput = PuzzleInput.parse(input)
  val part1 = solvePart1(puzzleInput)
  println(s"Part 1: $part1")
  val part2 = solvePart2(puzzleInput)
  println(s"Part 2: $part2")
