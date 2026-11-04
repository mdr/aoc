package aoc2021

import aoc.util.*

val numbers: BinaryNumberCollection = BinaryNumberCollection(
  loadLines("aoc2021/input-2021-3.txt").map(BinaryNumber.apply)
)

def day03Part1: Long = numbers.powerConsumption

type Bit = Char
case class BinaryNumberCollection(numbers: Seq[BinaryNumber]):
  assert(numbers.nonEmpty)

  def numberLength = numbers(0).length

  def gammaRate: Long =
    BinaryNumber((0 until numberLength).map(mostCommonBit).mkString).toLong

  def epsilonRate: Long =
    BinaryNumber((0 until numberLength).map(leastCommonBit).mkString).toLong

  def powerConsumption: Long = gammaRate * epsilonRate

  def bitSum(i: Int) =
    numbers.map(_.bitAt(i)).sumBy {
      case '0' => -1
      case '1' => 1
    }

  def mostCommonBit(i: Int) = if bitSum(i) >= 0 then '1' else '0'

  def leastCommonBit(i: Int) = if bitSum(i) >= 0 then '0' else '1'

  def filterWithBit(i: Int, c: Bit): BinaryNumberCollection = BinaryNumberCollection(numbers.filter(_.bitAt(i) == c))

  def oxygenGeneratorRating: Long = oxygenGeneratorRating(0).toLong

  private def oxygenGeneratorRating(i: Int): BinaryNumber =
    if numbers.length == 1 then numbers(0)
    else filterWithBit(i, mostCommonBit(i)).oxygenGeneratorRating(i + 1)

  def co2ScrubberRating: Long = co2ScrubberRating(0).toLong

  private def co2ScrubberRating(i: Int): BinaryNumber =
    if numbers.length == 1 then numbers(0)
    else filterWithBit(i, leastCommonBit(i)).co2ScrubberRating(i + 1)

  def lifeSupportRating: Long = oxygenGeneratorRating * co2ScrubberRating

case class BinaryNumber(bitString: String):

  def bitAt(i: Int): Bit = bitString(i)

  def length = bitString.length

  def toLong: Long = java.lang.Long.parseLong(bitString, 2)

def day03Part2: Long = numbers.lifeSupportRating
