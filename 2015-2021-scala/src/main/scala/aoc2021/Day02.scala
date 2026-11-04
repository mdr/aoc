package aoc2021

import aoc.util.*

val instructions = loadLines("aoc2021/input-2021-2.txt").map(SubInstructionParser.apply)

def day02Part1: Long = calculatePart1(instructions)

def calculatePart1(instructions: Seq[SubInstruction]): Long =
  val Point(horizontal, depth) = instructions.foldLeft(Point.Origin) { case (point, instruction) =>
    instruction.modify(point)
  }
  horizontal * depth

def day02Part2: Long = calculatePart2(instructions)

def calculatePart2(instructions: Seq[SubInstruction]): Long =
  val SubState(Point(horizontal, depth), _) = instructions.foldLeft(SubState.Initial)(_ modify _)
  horizontal * depth

sealed trait SubInstruction {

  def change: Point = this match
    case Forward(n) => Point(n, 0)
    case Down(n)    => Point(0, n)
    case Up(n)      => Point(0, -n)

  def modify(position: Point): Point = position + change

}

case class Forward(n: Int) extends SubInstruction
case class Down(n: Int)    extends SubInstruction
case class Up(n: Int)      extends SubInstruction

object SubInstructionParser extends DefaultParser:
  def apply(s: String): SubInstruction            = parseWithExceptions(subInstruction, s)
  lazy val subInstruction: Parser[SubInstruction] = forward | down | up
  lazy val forward: Parser[Forward]               = "forward" ~> integer ^^ Forward.apply
  lazy val down: Parser[Down]                     = "down" ~> integer ^^ Down.apply
  lazy val up: Parser[Up]                         = "up" ~> integer ^^ Up.apply

case class SubState(position: Point, aim: Int):

  def modify(instruction: SubInstruction): SubState =
    val Point(horizontal, depth) = position
    instruction match
      case Forward(n) => copy(position = Point(horizontal + n, depth + aim * n))
      case Down(n)    => copy(aim = aim + n)
      case Up(n)      => copy(aim = aim - n)

object SubState {
  val Initial = SubState(Point.Origin, 0)
}
