package aoc2015

import aoc.util.*
import scala.util.parsing.combinator.RegexParsers

opaque type Wire = String

object Wire {
  def apply(s: String): Wire = s
}

def organiseParts(partsAndWires: Seq[PartAndOutputWire]): Map[Wire, Part] = 
    partsAndWires.map { case PartAndOutputWire(part, wire) => wire -> part }.toMap

val partsAndWires = organiseParts(loadLines("aoc2015/input-2015-7.txt").map(PartParser(_)))

def day07Part1: Int =
  val emulator = new CircuitEmulator(partsAndWires)
  emulator.probe("a")

def day07Part2: Int =
  val emulator = new CircuitEmulator(partsAndWires + ("b" -> ConstantSignal(ConstantInput(day07Part1))))
  emulator.probe("a")

class CircuitEmulator(partsByOutputWire: Map[Wire, Part]) {

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
      case ConstantSignal(signal)       => getSignal(signal)
      case AndGate(input1, input2)      => getSignal(input1) & getSignal(input2)
      case OrGate(input1, input2)       => getSignal(input1) | getSignal(input2)
      case LeftShiftGate(input, shift)  => getSignal(input) << shift
      case RightShiftGate(input, shift) => getSignal(input) >> shift
      case NotGate(input)               => ~getSignal(input)
    val finalValue = signal & 0xffff
    _signals = _signals + (wire -> finalValue)
    finalValue

}

sealed trait GateInput
case class WireInput(wire: Wire)      extends GateInput
case class ConstantInput(signal: Int) extends GateInput

case class PartAndOutputWire(part: Part, output: Wire)

sealed trait Part
case class ConstantSignal(input: GateInput)              extends Part
case class AndGate(input1: GateInput, input2: GateInput) extends Part
case class OrGate(input1: GateInput, input2: GateInput)  extends Part
case class NotGate(input: GateInput)                     extends Part
case class LeftShiftGate(input: GateInput, shift: Int)   extends Part
case class RightShiftGate(input: GateInput, shift: Int)  extends Part

object PartParser extends DefaultParser:
  lazy val wire: Parser[Wire]       = regex("[a-z]+".r) ^^ (Wire(_))
  lazy val input: Parser[GateInput] = wire ^^ (WireInput(_)) | (integer ^^ (ConstantInput(_)))
  lazy val constantSignal: Parser[ConstantSignal] = input ^^ { case signal =>
    ConstantSignal(signal)
  }
  lazy val andGate: Parser[AndGate] = ((input <~ "AND") ~ input) ^^ { case input1 ~ input2 =>
    AndGate(input1, input2)
  }
  lazy val orGate: Parser[OrGate] = ((input <~ "OR") ~ input) ^^ { case input1 ~ input2 =>
    OrGate(input1, input2)
  }
  lazy val notGate: Parser[NotGate] = ("NOT" ~> input) ^^ { case input =>
    NotGate(input)
  }
  lazy val leftShiftGate: Parser[LeftShiftGate] = ((input <~ "LSHIFT") ~ integer) ^^ {
    case input ~ shift =>
      LeftShiftGate(input, shift)
  }
  lazy val rightShiftGate: Parser[RightShiftGate] = ((input <~ "RSHIFT") ~ integer) ^^ {
    case input ~ shift =>
      RightShiftGate(input, shift)
  }
  lazy val part: Parser[Part] = andGate | orGate | notGate | leftShiftGate | rightShiftGate | constantSignal
  lazy val partAndOutputWire: Parser[PartAndOutputWire] = (part <~ "->") ~ wire ^^ { case part ~ outputWire => PartAndOutputWire(part, outputWire) }
  
  def apply(s: String): PartAndOutputWire = parseWithExceptions(partAndOutputWire, s)
