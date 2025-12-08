package aoc2025.day08

import aoc2025.utils.{foldUntil, readInput}

case class Point3D(x: Double, y: Double, z: Double):
  def distanceTo(other: Point3D): Double =
    math.sqrt(
      math.pow(x - other.x, 2) +
        math.pow(y - other.y, 2) +
        math.pow(z - other.z, 2)
    )

object Point3D:
  given Ordering[Point3D] = Ordering.by(p => (p.x, p.y, p.z))

  private def parse(s: String): Point3D =
    val Seq(x, y, z) = s.split(",").map(_.trim.toDouble).toSeq
    Point3D(x, y, z)

  def parsePoints(input: String): Seq[Point3D] =
    input.linesIterator.map(Point3D.parse).toSeq

  def findClosestPairs(points: Set[Point3D]): Seq[(Point3D, Point3D)] =
    points.subsets(2).map { pair =>
      val Seq(p1, p2) = pair.toSeq.sorted
      (p1, p2)
    }.toSeq.sortBy { case (p1, p2) => p1.distanceTo(p2) }

case class Circuit(boxes: Set[Point3D]):
  def contains(box: Point3D): Boolean = boxes.contains(box)

  def merge(other: Circuit): Circuit =
    Circuit(this.boxes ++ other.boxes)

case class Playground(allBoxes: Set[Point3D], circuits: Set[Circuit]):
  def score: Long = circuits.toSeq.map(_.boxes.size.toLong).sorted.takeRight(3).product

  private def getCircuitContaining(box: Point3D): Circuit =
    circuits.find(_.contains(box)) getOrElse
      (throw new IllegalArgumentException(s"Box $box not found in any circuit"))

  private def connectBoxes(box1: Point3D, box2: Point3D): Playground =
    val circuit1 = getCircuitContaining(box1)
    val circuit2 = getCircuitContaining(box2)
    val newCircuit = circuit1.merge(circuit2)
    val newCircuits = circuits - circuit1 - circuit2 + newCircuit
    this.copy(circuits = newCircuits)

  def connect(numberOfBoxesToConnect: Int): Playground =
    val closestPairs = Point3D.findClosestPairs(allBoxes).take(numberOfBoxesToConnect)
    closestPairs.foldLeft(this) { case (playground, (box1, box2)) =>
      playground.connectBoxes(box1, box2)
    }

  def firstConnectionToFormSingleCircuit(): (Point3D, Point3D) =
    Point3D.findClosestPairs(allBoxes).foldUntil(this)(
      combine = { case (playground, (box1, box2)) => playground.connectBoxes(box1, box2) },
      stopWhen = _.circuits.size == 1
    ).stopElementOption.getOrElse(throw new IllegalStateException("Could not find a connection that forms a single circuit"))

def solvePart1(boxes: Set[Point3D], numberOfBoxesToConnect: Int) =
  val playground = Playground.initial(boxes)
  val finalPlayground = playground.connect(numberOfBoxesToConnect)
  finalPlayground.score

def solvePart2(boxes: Set[Point3D]): Long =
  val playground = Playground.initial(boxes)
  val (box1, box2) = playground.firstConnectionToFormSingleCircuit()
  box1.x.toLong * box2.x.toLong

object Playground:
  def initial(boxes: Set[Point3D]): Playground =
    val circuits = boxes.map(box => Circuit(Set(box)))
    Playground(allBoxes = boxes, circuits = circuits)

@main def day08(): Unit =
  val input = readInput("day08/input.txt")
  val boxes = Point3D.parsePoints(input).toSet

  val part1 = solvePart1(boxes, numberOfBoxesToConnect = 1000)
  println(s"Part 1: $part1")

  val part2 = solvePart2(boxes)
  println(s"Part 2: $part2")
