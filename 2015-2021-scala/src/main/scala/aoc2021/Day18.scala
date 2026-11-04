package aoc2021

import aoc.util.*
import aoc2021.SnailfishNumber

val snailfishNumbers = loadLines("aoc2021/input-2021-18.txt").map(SnailfishNumber.apply)

def day18Part1: Long = solveDay18Part1(snailfishNumbers)

def day18Part2: Long = solveDay18Part2(snailfishNumbers)

def solveDay18Part1(numbers: Seq[SnailfishNumber]): Long = numbers.reduce(_ + _).magnitude

def solveDay18Part2(numbers: Seq[SnailfishNumber]): Long = 
  val sums = 
    for 
      n1 <- numbers
      n2 <- numbers
    yield n1 + n2
  sums.map(_.magnitude).max
  

enum Choice:
  case Left, Right

object Path:
  def Root = Path(List.empty)

  def apply(s: String): Path = Path(s.toList.map {
    case 'L' => Choice.Left
    case 'R' => Choice.Right
  })

case class Path(choices: List[Choice]):

  def +:(choice: Choice): Path = Path(choice +: choices)
  def :+(choice: Choice): Path = Path(choices :+ choice)

  def length: Int = choices.length

  override def toString: String = choices.map {
    case Choice.Left  => "L"
    case Choice.Right => "R"
  }.mkString

  def isEmpty: Boolean = choices.isEmpty

object SnailfishNumber:
  def apply(s: String): SnailfishNumber = SnailfishNumberParser.parse(s)

case class ExplodeCandidate(path: Path, leftValue: Long, rightValue: Long)

def splitNumber(n: Long): (Long, Long) =
  (n.toDouble / 2).floor.toLong -> (n.toDouble / 2).ceil.toLong

sealed trait SnailfishNumber:
  def paths: Seq[Path]     = getPaths(Path.Root)
  def leafPaths: Seq[Path] = getLeafPaths(Path.Root)
  def getPaths(path: Path): Seq[Path]
  def getLeafPaths(path: Path): Seq[Path]
  def replace(path: Path, replacement: SnailfishNumber): SnailfishNumber
  def add(path: Path, n: Long): SnailfishNumber
  def tryExplode: Option[SnailfishNumber] = findPairToExplode().map(explode)
  def magnitude: Long
  def +(that: SnailfishNumber): SnailfishNumber = Pair(this, that).reduce

  def tryReduce: Option[SnailfishNumber] = tryExplode orElse trySplit

  def reduce: SnailfishNumber = tryReduce.fold(this)(_.reduce)

  def explode(explodeCandidate: ExplodeCandidate): SnailfishNumber =
    val ExplodeCandidate(path, leftValue, rightValue) = explodeCandidate
    val n1                                            = replace(path, RegularNumber(0))
    val n2 = n1.previousLeafPath(path).fold(n1)(previousPath => n1.add(previousPath, leftValue))
    val n3 = n2.nextLeafPath(path).fold(n2)(previousPath => n2.add(previousPath, rightValue))
    n3

  def findPairToExplode(path: Path = Path.Root): Option[ExplodeCandidate] = this match
    case RegularNumber(n) => None
    case Pair(RegularNumber(leftValue), RegularNumber(rightValue)) =>
      if path.length >= 4 then Some(ExplodeCandidate(path, leftValue, rightValue))
      else None
    case Pair(left, right) =>
      left.findPairToExplode(path :+ Choice.Left) orElse right.findPairToExplode(path :+ Choice.Right)

  def trySplit: Option[SnailfishNumber] =
    this match
      case RegularNumber(n) if n >= 10 =>
        val (leftValue, rightValue) = splitNumber(n)
        Some(Pair(RegularNumber(leftValue), RegularNumber(rightValue)))
      case RegularNumber(n) => None
      case Pair(left, right) =>
        left.trySplit.map(newLeft => Pair(newLeft, right)) orElse
          right.trySplit.map(newRight => Pair(left, newRight))

  def nextLeafPath(path: Path): Option[Path] =
    val paths = leafPaths
    paths.indexOfOption(path).flatMap { index =>
      val nextIndex = index + 1
      if nextIndex >= paths.length then None else Some(paths(nextIndex))
    }

  def previousLeafPath(path: Path): Option[Path] =
    val paths = leafPaths
    paths.indexOfOption(path).flatMap { index =>
      val previousIndex = index - 1
      if previousIndex < 0 then None else Some(paths(previousIndex))
    }

case class RegularNumber(n: Long) extends SnailfishNumber:
  override def toString               = n.toString
  def magnitude                       = n
  def getPaths(path: Path): Seq[Path] = Seq(path)

  def getLeafPaths(path: Path): Seq[Path] = Seq(path)
  def replace(path: Path, replacement: SnailfishNumber): SnailfishNumber =
    if path.isEmpty then replacement
    else throw new RuntimeException(s"Unexpected replacement of regular number at path $path")

  def add(path: Path, n: Long): SnailfishNumber =
    if path.isEmpty then copy(n = this.n + n)
    else throw new RuntimeException(s"Unexpected addition at path $path")

case class Pair(left: SnailfishNumber, right: SnailfishNumber) extends SnailfishNumber:
  override def toString = s"[$left,$right]"
  def magnitude         = 3 * left.magnitude + 2 * right.magnitude
  def getPaths(path: Path): Seq[Path] =
    left.getPaths(path :+ Choice.Left) ++ Seq(path) ++ right.getPaths(path :+ Choice.Right)

  def getLeafPaths(path: Path): Seq[Path] =
    left.getLeafPaths(path :+ Choice.Left) ++ right.getLeafPaths(path :+ Choice.Right)

  def replace(path: Path, replacement: SnailfishNumber): SnailfishNumber =
    path.choices match {
      case Nil                  => replacement
      case Choice.Left :: rest  => copy(left = left.replace(Path(rest), replacement))
      case Choice.Right :: rest => copy(right = right.replace(Path(rest), replacement))
    }

  def add(path: Path, n: Long): SnailfishNumber =
    path.choices match {
      case Nil                  => throw new RuntimeException("Cannot add pair")
      case Choice.Left :: rest  => copy(left = left.add(Path(rest), n))
      case Choice.Right :: rest => copy(right = right.add(Path(rest), n))
    }

object SnailfishNumberParser extends DefaultParser:
  def snailfishNumber: Parser[SnailfishNumber] = pair | regularNumber

  def pair: Parser[Pair] = (("[" ~> snailfishNumber) <~ ",") ~ (snailfishNumber <~ "]")
    ^^ { case left ~ right => Pair(left, right) }

  def regularNumber = long ^^ RegularNumber.apply

  def parse(s: String): SnailfishNumber = parseWithExceptions(snailfishNumber, s)
