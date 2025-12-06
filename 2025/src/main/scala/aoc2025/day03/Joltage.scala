package aoc2025.day03

opaque type Joltage = Long

object Joltage:
  def apply(value: Long): Joltage = value
  val Zero: Joltage = 0L

  extension (j: Joltage)
    def toLong: Long = j
    def +(other: Joltage): Joltage = j + other
    def *(other: Long): Joltage = j * other

  given Ordering[Joltage] = Ordering.Long
  given Numeric[Joltage] = Numeric.LongIsIntegral
