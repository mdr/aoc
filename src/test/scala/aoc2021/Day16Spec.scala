package aoc2021

import aoc.util.*
import aoc.UnitTest

class Day16Spec extends UnitTest:

  "Stuff" should "work" in {
    hexToBinary("D2FE28") shouldEqual "110100101111111000101000"

    PacketParser.parsePacket("110100101111111000101000") shouldEqual
      LiteralPacket(6, 4, 2021)

    PacketParser.parsePacket("00111000000000000110111101000101001010010001001000000000") shouldEqual
      OperatorPacket(
        1,
        6,
        Seq(
          LiteralPacket(6, 4, 10),
          LiteralPacket(2, 4, 20),
        )
      )

    PacketParser.parsePacket("11101110000000001101010000001100100000100011000001100000") shouldEqual
      OperatorPacket(
        7,
        3,
        Seq(
          LiteralPacket(2, 4, 1),
          LiteralPacket(4, 4, 2),
          LiteralPacket(1, 4, 3)
        )
      )
    solveDay16Part1("8A004A801A8002F478") shouldEqual 16
    solveDay16Part1("620080001611562C8802118E34") shouldEqual 12
    solveDay16Part1("C0015000016115A2E0802F182340") shouldEqual 23
    solveDay16Part1("A0016C880162017C3686B18A3D4780") shouldEqual 31

    solveDay16Part2("C200B40A82") shouldEqual 3
    solveDay16Part2("04005AC33890") shouldEqual 54
    solveDay16Part2("880086C3E88112") shouldEqual 7
    solveDay16Part2("CE00C43D881120") shouldEqual 9
    solveDay16Part2("D8005AC2A8F0") shouldEqual 1
  }


  "Solution to part 1" should "be correct" in {
    day16Part1 shouldEqual 901
  }

  "Solution to part 2" should "be correct" in {
    day16Part2 shouldEqual 110434737925L
  }
