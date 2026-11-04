package aoc2021

import aoc.util.*
import scala.util.parsing.input.Positional
import scala.util.chaining._

val hex = loadString("aoc2021/input-2021-16.txt").stripLineEnd

def day16Part1: Int  = solveDay16Part1(hex)
def day16Part2: Long = solveDay16Part2(hex)

def solveDay16Part1(hex: String): Int  = Packet.parse(hex).versionSum
def solveDay16Part2(hex: String): Long = Packet.parse(hex).value

object Packet:
  def parse(hex: String): Packet = PacketParser.parsePacket(hexToBinary(hex))

sealed trait Packet:
  val version: Int
  val typeId: Int
  def versionSum: Int
  def value: Long

case class LiteralPacket(version: Int, typeId: Int, value: Long) extends Packet:
  def versionSum = version

case class OperatorPacket(version: Int, typeId: Int, packets: Seq[Packet]) extends Packet:
  def versionSum = version + packets.sumBy(_.versionSum)
  def value =
    val values                               = packets.map(_.value)
    def binaryOp(f: (Long, Long) => Boolean) = values.pipe { case Seq(a, b) => if f(a, b) then 1 else 0 }
    typeId match
      case 0 => values.sum
      case 1 => values.product
      case 2 => values.min
      case 3 => values.max
      case 5 => binaryOp(_ > _)
      case 6 => binaryOp(_ < _)
      case 7 => binaryOp(_ == _)

def hexToBinary(s: String): String =
  val raw = BigInt(s, 16).toString(2)
  raw.reverse.padTo(s.length * 4, '0').reverse

def intToBinary(n: Int): String = BigInt(n).toString(2)

def binaryToInt(s: String): Int = Integer.parseInt(s, 2)

def binaryToLong(s: String): Long = java.lang.Long.parseLong(s, 2)

object PacketParser extends DefaultParser:

  lazy val bit = "0" | "1"

  private def toInt(bits: Seq[String]): Int = binaryToInt(bits.mkString)

  lazy val version: Parser[Int] = repN(3, bit) ^^ toInt

  lazy val typeId: Parser[Int] = repN(3, bit) ^^ toInt

  lazy val quad = repN(4, bit)

  lazy val literal: Parser[Long] = (rep("1" ~> quad) ~ ("0" ~> quad)) ^^ { case quads ~ finalQuad =>
    binaryToLong((quads.flatten ++ finalQuad).mkString)
  }

  lazy val literalPacket: Parser[LiteralPacket] = version ~ typeId.filter(_ == 4) ~ literal ^^ {
    case version ~ typeId ~ literal => LiteralPacket(version, typeId, literal)
  }

  lazy val subpackets: Parser[Seq[Packet]] = type0Subpackets | type1Subpackets

  lazy val type0Subpackets: Parser[Seq[Packet]] = "0" ~> (repN(15, bit) ^^ toInt).into(packetsOfLength)

  def packetsOfLength(n: Int): Parser[Seq[Packet]] = (repN(n, bit) ^^ (_.mkString)) ^^ PacketParser.parsePackets

  lazy val type1Subpackets: Parser[Seq[Packet]] =
    "1" ~> (repN(11, bit) ^^ toInt).into(n => repN(n, packet))

  lazy val operatorPacket: Parser[OperatorPacket] = version ~ typeId.filter(_ != 4) ~ subpackets ^^ {
    case version ~ typeId ~ packets => OperatorPacket(version, typeId, packets)
  }

  lazy val packet: Parser[Packet] = literalPacket | operatorPacket

  def parsePacket(s: String) = parseWithExceptions(packet, s.mkString(" "))

  def parsePackets(s: String): Seq[Packet] = parseWithExceptions(rep1(packet), s.mkString(" "))
