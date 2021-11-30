package aoc2015

import aoc.util.*
import scala.util.parsing.combinator.RegexParsers

val parts = loadLines("aoc2015/input-2015-7.txt").map(PartParser(_))

def day07Part1: Int =
  val emulator = new CircuitEmulator(parts)
  emulator.probe("a")

def day07Part2: Int =
  val emulator = new CircuitEmulator(organiseParts(parts) + ("b" -> ConstantSignal(ConstantInput(day07Part1), "b")))
  emulator.probe("a")

opaque type Wire = String

object Wire {
  def apply(s: String): Wire = s
}

def organiseParts(parts: Seq[Part]): Map[Wire, Part] = parts.map(part => part.output -> part).toMap

class CircuitEmulator(partsByOutputWire: Map[Wire, Part]) {
  def this(parts: Seq[Part]) = this(organiseParts(parts))

  private var _signals: Map[Wire, Int] = Map()

  def probe(wire: Wire): Int = _signals.get(wire) getOrElse calculateSignal(wire)

  private def getSignal(input: GateInput): Int =
    input match
      case ConstantInput(signal) => signal
      case WireInput(wire)       => probe(wire)

  def signals: Map[Wire, Int] =
    partsByOutputWire.keys.foreach(probe)
    _signals

  private def calculateSignal(wire: Wire): Int =
    val signal = partsByOutputWire(wire) match
      case ConstantSignal(signal, _)       => getSignal(signal)
      case AndGate(input1, input2, _)      => getSignal(input1) & getSignal(input2)
      case OrGate(input1, input2, _)       => getSignal(input1) | getSignal(input2)
      case LeftShiftGate(input, shift, _)  => getSignal(input) << shift
      case RightShiftGate(input, shift, _) => getSignal(input) >> shift
      case NotGate(input, _)               => ~getSignal(input)
    val finalValue = signal & 0xffff
    _signals = _signals + (wire -> finalValue)
    finalValue

}

sealed trait GateInput
case class WireInput(wire: Wire)      extends GateInput
case class ConstantInput(signal: Int) extends GateInput

sealed trait Part:
  val output: Wire

case class ConstantSignal(input: GateInput, output: Wire)              extends Part
case class AndGate(input1: GateInput, input2: GateInput, output: Wire) extends Part
case class OrGate(input1: GateInput, input2: GateInput, output: Wire)  extends Part
case class NotGate(input: GateInput, output: Wire)                     extends Part
case class LeftShiftGate(input: GateInput, shift: Int, output: Wire)   extends Part
case class RightShiftGate(input: GateInput, shift: Int, output: Wire)  extends Part

object PartParser extends DefaultParser:
  lazy val wire: Parser[Wire]       = regex("[a-z]+".r) ^^ (Wire(_))
  lazy val input: Parser[GateInput] = wire ^^ (WireInput(_)) | (integer ^^ (ConstantInput(_)))
  lazy val constantSignal: Parser[ConstantSignal] = (input <~ "->") ~ wire ^^ { case signal ~ output =>
    ConstantSignal(signal, output)
  }
  lazy val andGate: Parser[AndGate] = (((input <~ "AND") ~ input) <~ "->") ~ wire ^^ { case input1 ~ input2 ~ output =>
    AndGate(input1, input2, output)
  }
  lazy val orGate: Parser[OrGate] = (((input <~ "OR") ~ input) <~ "->") ~ wire ^^ { case input1 ~ input2 ~ output =>
    OrGate(input1, input2, output)
  }
  lazy val notGate: Parser[NotGate] = (("NOT" ~> input) <~ "->") ~ wire ^^ { case input ~ output =>
    NotGate(input, output)
  }
  lazy val leftShiftGate: Parser[LeftShiftGate] = (((input <~ "LSHIFT") ~ integer) <~ "->") ~ wire ^^ {
    case input ~ shift ~ output =>
      LeftShiftGate(input, shift, output)
  }
  lazy val rightShiftGate: Parser[RightShiftGate] = (((input <~ "RSHIFT") ~ integer) <~ "->") ~ wire ^^ {
    case input ~ shift ~ output =>
      RightShiftGate(input, shift, output)
  }
  lazy val part: Parser[Part] = constantSignal | andGate | orGate | notGate | leftShiftGate | rightShiftGate

  def apply(s: String): Part = parseWithExceptions(part, s)
