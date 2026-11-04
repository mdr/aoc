package aoc2025.day10

import aoc2025.utils.{AStar, readInput, sumBy}

import scala.annotation.tailrec

// [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
// [...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
// [.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
// Each line contains a single indicator light diagram in [square brackets], one or more button
// wiring schematics in (parentheses), and joltage requirements in {curly braces}.

enum LightState:
  case On
  case Off

  def toggle: LightState = this match
    case LightState.On => LightState.Off
    case LightState.Off => LightState.On

object LightState:
  def parse(c: Char): LightState = c match
    case '#' => LightState.On
    case '.' => LightState.Off
    case _ => throw new IllegalArgumentException(s"Invalid light state character: $c")

case class IndicatorLights(lights: Seq[LightState]):
  def toggle(positions: Seq[Int]): IndicatorLights =
    val newLights = lights.zipWithIndex.map { case (state, index) =>
      if positions.contains(index) then state.toggle else state
    }
    IndicatorLights(newLights)

  def size: Int = lights.size

object IndicatorLights:
  def parse(s: String): IndicatorLights =
    val content = s.substring(1, s.length - 1)
    val lights = content.map(LightState.parse)
    IndicatorLights(lights)

  def allOff(n: Int): IndicatorLights =
    IndicatorLights(List.fill(n)(LightState.Off))

case class Button(positions: Seq[Int])

object Button:
  def parse(s: String): Button =
    val content = s.substring(1, s.length - 1)
    val positions = content.split(",").map(_.trim.toInt).toSeq
    Button(positions)

case class Joltage(counters: Seq[Int]):

  def increment(positions: Seq[Int]): Joltage =
    Joltage(counters.zipWithIndex.map({ case (counter, position) =>
      if positions contains position then counter + 1 else counter
    }))

object Joltage:
  def parse(s: String): Joltage =
    val content = s.substring(1, s.length - 1)
    val requirements = content.split(",").map(_.trim.toInt).toSeq
    Joltage(requirements)

  def initial(n: Int): Joltage = Joltage(List.fill(n)(0))

  def heuristicDistance(current: Joltage, target: Joltage): Double =
    current.counters.zip(target.counters).map { case (curr, targ) => (targ - curr).toDouble }.min

case class Machine(targetLights: IndicatorLights, buttons: Seq[Button], joltageRequirements: Joltage):

  def fewestTotalPressesToTargetLights: Long =
    def search(boundary: Set[IndicatorLights], pressesSoFar: Long = 0): Long =
      val updatedBoundary =
        for {
          lights <- boundary
          button <- buttons
          updatedLights = lights.toggle(button.positions)
        } yield updatedLights
      if updatedBoundary contains targetLights then
        pressesSoFar + 1
      else
        search(updatedBoundary, pressesSoFar + 1)

    search(Set(IndicatorLights.allOff(targetLights.size)))

  private def hasExceededJoltageRequirements(joltage: Joltage): Boolean =
    joltage.counters.zip(joltageRequirements.counters).exists { case (current, required) => current > required }

  def fewestTotalPressesToTargetJoltage: Double = {
    println(s"Starting A* search for joltage requirements: $joltageRequirements")
    AStar.findMinCost(
      start = Joltage.initial(joltageRequirements.counters.size),
      goal = joltageRequirements,
      neighbors = joltage =>
        buttons.map(button => (joltage.increment(button.positions), 1.0)),
      heuristic = joltage =>
        if hasExceededJoltageRequirements(joltage) then Double.PositiveInfinity
        else
          Joltage.heuristicDistance(joltage, joltageRequirements)
    ).getOrElse(throw new IllegalStateException("No path found to reach target joltage"))
  }

  def fewestTotalPressesToTargetJoltage2: Long =
    @tailrec
    def search(boundary: Set[Joltage], pressesSoFar: Long = 0): Long =
      println(s"Presses so far: $pressesSoFar, boundary size: ${boundary.size}")
      val updatedBoundary =
        for {
          joltage <- boundary
          button <- buttons
          updatedJoltage = joltage.increment(button.positions)
          if !hasExceededJoltageRequirements(updatedJoltage)
        } yield updatedJoltage
      if updatedBoundary contains joltageRequirements then
        pressesSoFar + 1
      else
        search(updatedBoundary, pressesSoFar + 1)

    search(Set(Joltage.initial(joltageRequirements.counters.size)))

object Machine:
  def parse(line: String): Machine =
    val lightPattern = """\[[^\]]+\]""".r
    val buttonPattern = """\([^\)]+\)""".r
    val joltagePattern = """\{[^\}]+\}""".r

    val lightMatch = lightPattern.findFirstIn(line).getOrElse(throw new IllegalArgumentException(s"No indicator lights found in line: $line"))
    val buttonMatches = buttonPattern.findAllIn(line).toSeq
    val joltageMatch = joltagePattern.findFirstIn(line).getOrElse(throw new IllegalArgumentException(s"No joltage requirements found in line: $line"))

    val lights = IndicatorLights.parse(lightMatch)
    val buttons = buttonMatches.map(Button.parse)
    val joltageRequirements = Joltage.parse(joltageMatch)

    Machine(lights, buttons, joltageRequirements)

  def parseMachines(input: String): Seq[Machine] =
    input.linesIterator.map(Machine.parse).toSeq

def solvePart1(machines: Seq[Machine]): Long = machines.sumBy(_.fewestTotalPressesToTargetLights)

def solvePart2(machines: Seq[Machine]): Double = machines.sumBy(_.fewestTotalPressesToTargetJoltage)

@main def day10(): Unit =
  val input = readInput("day10/input.txt")
  val machines = Machine.parseMachines(input)

  val part1 = solvePart1(machines)
  println(s"Part 1: $part1")

  val part2 = solvePart2(machines)
  println(s"Part 2: $part2")
