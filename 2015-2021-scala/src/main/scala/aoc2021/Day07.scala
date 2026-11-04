package aoc2021

import aoc.util.*

val crabs = Crabs.parse(loadString("aoc2021/input-2021-7.txt"))

def day07Part1: Int = crabs.optimise(FuelCost.Constant).fuel
def day07Part2: Int = crabs.optimise(FuelCost.Increasing).fuel

object Crabs:
  def parse(s: String) = Crabs(s.trim.split(",").map(_.toInt))

case class PositionAndFuel(position: Int, fuel: Int)

type FuelCost = Int => Int

object FuelCost:
  val Constant: FuelCost   = identity
  val Increasing: FuelCost = triangularNumber

case class Crabs(positions: Seq[Int]):

  def optimise(fuelCost: FuelCost): PositionAndFuel =
    val candidatePositions = positions.min to positions.max
    val fuelNeeded         = fuelNeededForAlignment(fuelCost)
    val optimalPosition    = candidatePositions minBy fuelNeeded
    PositionAndFuel(optimalPosition, fuelNeeded(optimalPosition))

  private def fuelNeededForAlignment(fuelCost: FuelCost)(position: Int): Int =
    positions.sumBy(p => fuelCost((p - position).abs))
