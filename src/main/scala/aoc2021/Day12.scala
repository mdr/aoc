package aoc2021

import aoc.util.*

val caveSystem = CaveSystem.parse(loadString("aoc2021/input-2021-12.txt"))

def day12Part1: Int = caveSystem.solvePart1

def day12Part2: Int = caveSystem.solvePart2

object CaveSystem:
  def parse(s: String): CaveSystem =
    def parseLine(line: String): (Cave, Cave) =
      line.trim.split("-").toSeq match
        case Seq(s1, s2) => Cave(s1) -> Cave(s2)
    val pairs = getLines(s.trim).map(_.trim).map(parseLine)
    val adjacency =
      (pairs ++ pairs.map(_.flip)).toSet
        .groupMapReduce(_._1) { case (_, cave) => Seq(cave) }(_ ++ _)
    CaveSystem(adjacency)

object Path:
  def Initial = Path(Seq(Cave.Start))

case class Path(caves: Seq[Cave]):
  def currentCave: Cave = caves.last

  def hasRevisitedSmallCave: Boolean = caves.filter(_.isSmall).counts.values.exists(_ > 1)

  def canVisit(mode: Mode)(cave: Cave): Boolean =
    mode match
      case Mode.Part1 => canVisit1(cave)
      case Mode.Part2 => canVisit2(cave)

  def canVisit1(cave: Cave): Boolean = cave.isBig || !caves.contains(cave)

  def canVisit2(cave: Cave): Boolean =
    (cave.isBig ||
      cave.isSmall && !(caves.contains(cave) && hasRevisitedSmallCave) && cave != Cave.Start)

  def visit(cave: Cave): Path = copy(caves = caves :+ cave)

  def maybeVisit(mode: Mode)(cave: Cave): Option[Path] =
    if canVisit(mode)(cave) then Some(visit(cave)) else None

  def isComplete: Boolean = currentCave == Cave.End

  override def toString = caves.map(_.name).mkString(",")

enum Mode:
  case Part1, Part2

case class CaveSystem(adjacency: Map[Cave, Seq[Cave]]):

  def spelunk(mode: Mode)(path: Path = Path.Initial): Seq[Path] =
    if path.isComplete then Seq(path)
    else
      getAdjacentCaves(path.currentCave)
        .flatMap(path.maybeVisit(mode))
        .flatMap(spelunk(mode))

  def getAdjacentCaves(cave: Cave): Seq[Cave] = adjacency.getOrElse(cave, Seq.empty)

  def solvePart1: Int = spelunk(Mode.Part1)().size

  def solvePart2: Int = spelunk(Mode.Part2)().size

object Cave {
  val Start = Cave("start")
  val End   = Cave("end")
}

case class Cave(name: String) {
  val isBig: Boolean   = name(0).isUpper
  val isSmall: Boolean = name(0).isLower
}
