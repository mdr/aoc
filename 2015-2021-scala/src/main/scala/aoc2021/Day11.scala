package aoc2021

import aoc.util.*
import scala.annotation.tailrec

val octoGrid = OctoGrid.parse(loadString("aoc2021/input-2021-11.txt"))

def day11Part1: Int = solveDay1(octoGrid)

def day11Part2: Int = solveDay2(octoGrid)

def solveDay1(octoGrid: OctoGrid): Int = iterate(octoGrid, 100)(_.step).totalFlashes

def solveDay2(octoGrid: OctoGrid): Int = iterateUntil(octoGrid, _.allHaveJustFlashed)(_.step).stepsTaken

object OctoGrid:

  def parse(s: String) =
    val rows       = getLines(s.trim).map(_.trim.map(_.toString.toInt))
    val gridWidth  = rows(0).size
    val gridHeight = rows.size
    val coords =
      for
        x <- 0 until gridWidth
        y <- 0 until gridHeight
        point = Point(x, y)
        value = rows(point.y.toInt)(point.x.toInt)
      yield point -> value
    OctoGrid(coords.toMap, gridWidth, gridHeight)

case class OctoGrid(
    energyLevels: Map[Point, Int],
    width: Int,
    height: Int,
    totalFlashes: Int = 0,
    readyToFlash: Set[Point] = Set.empty,
    stepsTaken: Int = 0
):

  private def incrementEnergy(point: Point): OctoGrid =
    val newEnergyLevel = energyLevels(point) + 1
    copy(
      energyLevels = energyLevels + (point -> newEnergyLevel),
      readyToFlash = if newEnergyLevel == 10 then readyToFlash + point else readyToFlash
    )

  private def incrementAllEnergy: OctoGrid = energyLevels.keySet.foldLeft(this)(_ incrementEnergy _)

  private def flashComplete(point: Point): OctoGrid =
    copy(readyToFlash = readyToFlash - point, totalFlashes = totalFlashes + 1)

  private def propagateFlash(point: Point): OctoGrid =
    val neighbours =
      for
        dx <- -1 to 1
        dy <- -1 to 1
        neighbour = point + Point(dx, dy)
        if energyLevels contains neighbour
      yield neighbour
    neighbours.foldLeft(this)(_ incrementEnergy _).flashComplete(point)

  @tailrec
  private def propagateFlashes: OctoGrid =
    if readyToFlash.isEmpty then this
    else
      val point = readyToFlash.head
      propagateFlash(point).propagateFlashes

  private def incrementStepsTaken = copy(stepsTaken = stepsTaken + 1)

  private def resetFlashedEnergyLevels: OctoGrid =
    val newEnergyLevels =
      for (point, level) <- energyLevels
      yield point -> (if level >= 10 then 0 else level)
    copy(energyLevels = newEnergyLevels)

  def step: OctoGrid = incrementAllEnergy.propagateFlashes.resetFlashedEnergyLevels.incrementStepsTaken

  def allHaveJustFlashed: Boolean = energyLevels.values.toSet == Set(0)
