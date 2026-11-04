package aoc2021

import aoc.util.*

val puzzleInput = PuzzleInput.parse(loadString("aoc2021/input-2021-13.txt"))

def day13Part1: Int = puzzleInput.solvePart1

def day13Part2 = puzzleInput.solvePart2

object Axis:
  def parse(s: String): Axis = s.trim match
    case "x" => Axis.X
    case "y" => Axis.Y

enum Axis:
  case X, Y

object Fold:
  def parse(s: String): Fold =
    val Seq(axis, coordinate) = s.trim.drop("fold along".length).split("=").toSeq
    Fold(Axis.parse(axis), coordinate.toInt)

case class Fold(axis: Axis, coordinate: Int):
  def apply(point: Point) = axis match
    case Axis.Y if point.y > coordinate => point.copy(y = coordinate - (point.y - coordinate))
    case Axis.X if point.x > coordinate => point.copy(x = coordinate - (point.x - coordinate))
    case _                              => point

object PuzzleInput:
  def parse(s: String): PuzzleInput =
    val Seq(dotSection, foldSection) = s.split("\n\n").toSeq
    def parsePoint(s: String) =
      val Seq(x, y) = s.trim.split(",").toSeq
      Point(x.toLong, y.toLong)
    val dots  = getLines(dotSection.trim).map(parsePoint)
    val folds = getLines(foldSection.trim).map(Fold.parse)
    PuzzleInput(dots, folds)

case class PuzzleInput(dots: Seq[Point], folds: Seq[Fold]):
  def paper = Paper(dots.toSet)

  def solvePart1 = paper.fold(folds.head).numberOfDots

  def solvePart2: String = folds.foldLeft(paper)(_ fold _).toString

case class Paper(dots: Set[Point]):
  lazy val width  = dots.map(_.x).max.toInt + 1
  lazy val height = dots.map(_.y).max.toInt + 1

  val numberOfDots = dots.size

  override def toString: String =
    def makeRow(y: Int): String =
      (0 until width)
        .map(x => if dots contains Point(x, y) then "#" else ".")
        .mkString
    (0 until height).map(makeRow).mkString("\n")

  def fold(fold: Fold): Paper = copy(dots = dots.map(fold.apply))
