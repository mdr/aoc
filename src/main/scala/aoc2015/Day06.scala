package aoc2015

import aoc.util.*
import scala.util.parsing.combinator.RegexParsers

import Day06._

def day06Part1: Int = gridPoints.count(calculateLightState)

def day06Part2: Int = gridPoints.sumBy(calculateBrightness)

object Day06:

  val gridPoints: Seq[Point] =
    for
      x <- 0 to 999
      y <- 0 to 999
    yield Point(x, y)

  val instructions = loadLines("aoc2015/input-2015-6.txt").map(InstructionParser(_))

  def calculateLightState(point: Point): Boolean = instructions.foldLeft(false) { case (light, instruction) =>
    if instruction.rectangle contains point then instruction.instructionType(light)
    else light
  }

  def calculateBrightness(point: Point): Int = instructions.foldLeft(0) { case (brightness, instruction) =>
    if instruction.rectangle contains point then instruction.instructionType.modifyBrightness(brightness)
    else brightness
  }

  enum InstructionType:

    def apply(light: Boolean): Boolean =
      this match
        case TurnOn  => true
        case TurnOff => false
        case Toggle  => !light

    def modifyBrightness(level: Int): Int =
      this match
        case TurnOn  => level + 1
        case TurnOff => (level - 1).max(0)
        case Toggle  => level + 2

    case TurnOn, TurnOff, Toggle

  case class Instruction(instructionType: InstructionType, rectangle: Rectangle)

  case class Rectangle(bottomLeft: Point, topRight: Point):
    def contains(point: Point): Boolean =
      point.x >= bottomLeft.x && point.y >= bottomLeft.y &&
        point.x <= topRight.x && point.y <= topRight.y

  object InstructionParser extends RegexParsers with WithParseWithExceptions:

    private lazy val instruction: Parser[Instruction] = instructionType ~ rectangle ^^ {
      case instructionType ~ rectangle =>
        Instruction(instructionType, rectangle)
    }

    private lazy val integer: Parser[Int] = """(0|[1-9]\d*)""".r ^^ (_.toInt)

    private lazy val point: Parser[Point] = (integer <~ ",") ~ integer ^^ { case (x ~ y) => Point(x, y) }

    private lazy val rectangle: Parser[Rectangle] = (point <~ "through") ~ point ^^ { case bottomLeft ~ topRight =>
      Rectangle(bottomLeft, topRight)
    }

    private lazy val instructionType: Parser[InstructionType] =
      ("turn" ~ "on") ^^^ InstructionType.TurnOn | ("turn" ~ "off") ^^^ InstructionType.TurnOff | "toggle" ^^^ InstructionType.Toggle

    def apply(s: String): Instruction = parseWithExceptions(instruction, s)
