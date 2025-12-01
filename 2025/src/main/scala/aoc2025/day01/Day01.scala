package aoc2025.day01

import aoc2025.readInput

sealed trait Rotation:
  def value: Int

object Rotation:
  case class Left(amount: Int) extends Rotation:
    override def value: Int = -amount

  case class Right(amount: Int) extends Rotation:
    override def value: Int = amount

  def parseRotation(input: String): Rotation =
    val direction = input.charAt(0)
    val amount = input.substring(1).toInt
    direction match
      case 'L' => Left(amount)
      case 'R' => Right(amount)
      case _ => throw new IllegalArgumentException(s"Invalid rotation: $input")

  def parseRotations(input: String): Seq[Rotation] =
    input.linesIterator.map(_.trim).filter(_.nonEmpty).map(Rotation.parseRotation).toSeq

case class Dial(position: Int):
  def rotate(rotation: Rotation) = Dial(mod(position + rotation.value, Dial.size))

object Dial:
  val size = 100
  val initial: Dial = Dial(50)

def mod(a: Int, b: Int): Int = ((a % b) + b) % b

case class ZeroCountingDial(position: Int, zeroPasses: Int = 0):

  def rotate(rotation: Rotation): ZeroCountingDial =
    val sum = position + rotation.value

    val newPosition = mod(sum, Dial.size)

    val div = sum / Dial.size
    var newZeroPasses = zeroPasses + div.abs
    if (position > 0 && sum <= 0)
      newZeroPasses += 1

    ZeroCountingDial(newPosition, newZeroPasses)

object ZeroCountingDial:
  val initial: ZeroCountingDial = ZeroCountingDial(position = 50)

def countZeros(rotations: Seq[Rotation]): Int =
  val trace = rotations.scanLeft(Dial.initial)((dial, rotation) => dial.rotate(rotation))
  trace.count(_.position == 0)

def countZeroPasses(rotations: Seq[Rotation]): Int =
  rotations.foldLeft(ZeroCountingDial.initial)((dial, rotation) => dial.rotate(rotation)).zeroPasses

@main def day01(): Unit =
  val rotations = Rotation.parseRotations(readInput("day01/input.txt"))
  val part1 = countZeros(rotations)
  println(s"Part 1: $part1")
  val part2 = countZeroPasses(rotations)
  println(s"Part 2: $part2")
