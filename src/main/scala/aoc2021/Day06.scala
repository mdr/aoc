package aoc2021

import aoc.util.*

val slowFishState = SlowFishState.parse(loadString("aoc2021/input-2021-6.txt"))

val fastFishState = FastFishState.parse(loadString("aoc2021/input-2021-6.txt"))

def day06Part1: Int = solvePart1(slowFishState)

def day06Part2: Long = solvePart2(fastFishState)

def solvePart1(fishState: SlowFishState): Int = iterate(fishState, 80)(_.evolve).size

def solvePart2(fishState: FastFishState): Long = iterate(fishState, 256)(_.evolve).size

case class Fish(daysLeft: Int):

  def evolve: (Fish, Option[Fish]) = daysLeft match
    case 0 => Fish(6)     -> Some(Fish(8))
    case n => Fish(n - 1) -> None

object Fish:
  def parseFishes(s: String): Seq[Fish] = s.trim.split(",").map(n => Fish(n.toInt))

case class SlowFishState(fishes: Seq[Fish]):

  def evolve: SlowFishState =
    val (originalFishes, newFishes) = fishes.map(_.evolve).unzip
    SlowFishState(originalFishes ++ newFishes.flatten)

  def size: Int = fishes.size

object SlowFishState:

  def parse(s: String) = SlowFishState(Fish.parseFishes(s))

case class FastFishState(fishCounts: Bag[Fish]):

  def evolve: FastFishState =
    val newFishCounts =
      fishCounts.bagFlatMap { case (fish, count) =>
        val (evolvedFish, maybeNewFish) = fish.evolve
        val evolvedFishBag              = Bag(evolvedFish -> count)
        val newFishBag                  = maybeNewFish.map(fish => Bag(fish -> count)) getOrElse Bag.empty
        evolvedFishBag ++ newFishBag
      }
    FastFishState(newFishCounts)

  def size: Long = fishCounts.size

object FastFishState:

  def parse(s: String) = FastFishState(Bag.of(Fish.parseFishes(s)))
