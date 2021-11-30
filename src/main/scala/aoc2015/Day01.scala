package aoc2015

import aoc.util.*

private val instructions = loadString("aoc2015/input-2015-1.txt")

def day01Part1: Int = followFloorInstructions(instructions)

def followFloorInstructions(s: String): Int =
  followFloorInstructions(parseInstructions(s), current = 0)

private def parseInstructions(s: String): List[Instruction] = s.toList.map(parseInstruction)

private def parseInstruction(c: Char): Instruction =
  c match {
    case '(' => Instruction.Up
    case ')' => Instruction.Down
    case c   => throw RuntimeException(s"Cannot parse instructions, unexpected character $c")
  }

private enum Instruction(val offset: Int):
  case Up extends Instruction(offset = 1)
  case Down extends Instruction(offset = -1)

private def followFloorInstructions(instructions: List[Instruction], current: Int): Int =
  instructions match
    case Nil                 => current
    case instruction :: rest => followFloorInstructions(rest, current + instruction.offset)

def firstInstructionPositionThatEntersBasement(s: String): Int =
  parseInstructions(s)
    .scanLeft(0)(_ + _.offset)
    .indexWhere(_ < 0)

def day01Part2: Int = firstInstructionPositionThatEntersBasement(instructions)
