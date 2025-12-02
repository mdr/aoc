import aoc2025.day01.{Rotation, countZeros, countZeroPasses, ZeroCountingDial}
import aoc2025.utils.readInput
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Day01Test extends AnyFunSpec with Matchers {
  describe("Parsing") {
    it("should parse rotations") {
      Rotation.parseRotation("L68") shouldBe Rotation.Left(68)
      Rotation.parseRotation("R48") shouldBe Rotation.Right(48)
    }

    it("should parse input") {
      val input =
        """L90
          |R180
          |L45""".stripMargin
      Rotation.parseRotations(input) shouldBe Seq(
        Rotation.Left(90),
        Rotation.Right(180),
        Rotation.Left(45)
      )
    }
  }

  val exampleInput: String =
    """L68
      |L30
      |R48
      |L5
      |R60
      |L55
      |L1
      |L99
      |R14
      |L82""".stripMargin

  val exampleRotations: Seq[Rotation] = Rotation.parseRotations(exampleInput)
  val puzzleRotations: Seq[Rotation] = Rotation.parseRotations(readInput("day01/input.txt"))

  describe("countZeros") {
    it("should count times at zero for example") {
      countZeros(exampleRotations) shouldBe 3
    }

    it("should count times at zero for puzzle input") {
      countZeros(puzzleRotations) shouldBe 1076
    }
  }

  describe("countZeroPasses") {
    it("should count times passed through zero for example") {
      countZeroPasses(exampleRotations) shouldBe 6
    }

    it("should count times passed through zero for puzzle input") {
      countZeroPasses(puzzleRotations) shouldBe 6379
    }
  }

  describe("ZeroCountingDial") {
    it("should count times at or passed zero") {
      ZeroCountingDial(position = 0).rotate(Rotation.Right(42)) shouldBe ZeroCountingDial(position = 42, zeroPasses = 0)
      ZeroCountingDial(position = 0).rotate(Rotation.Right(100)) shouldBe ZeroCountingDial(position = 0, zeroPasses = 1)
      ZeroCountingDial(position = 0).rotate(Rotation.Right(142)) shouldBe ZeroCountingDial(position = 42, zeroPasses = 1)
      ZeroCountingDial(position = 0).rotate(Rotation.Right(200)) shouldBe ZeroCountingDial(position = 0, zeroPasses = 2)

      ZeroCountingDial(position = 0).rotate(Rotation.Left(42)) shouldBe ZeroCountingDial(position = 58, zeroPasses = 0)
      ZeroCountingDial(position = 0).rotate(Rotation.Left(100)) shouldBe ZeroCountingDial(position = 0, zeroPasses = 1)
      ZeroCountingDial(position = 0).rotate(Rotation.Left(142)) shouldBe ZeroCountingDial(position = 58, zeroPasses = 1)
      ZeroCountingDial(position = 0).rotate(Rotation.Left(200)) shouldBe ZeroCountingDial(position = 0, zeroPasses = 2)

      ZeroCountingDial(position = 10).rotate(Rotation.Right(42)) shouldBe ZeroCountingDial(position = 52, zeroPasses = 0)
      ZeroCountingDial(position = 10).rotate(Rotation.Right(90)) shouldBe ZeroCountingDial(position = 0, zeroPasses = 1)
      ZeroCountingDial(position = 10).rotate(Rotation.Right(142)) shouldBe ZeroCountingDial(position = 52, zeroPasses = 1)

      ZeroCountingDial(position = 10).rotate(Rotation.Left(40)) shouldBe ZeroCountingDial(position = 70, zeroPasses = 1)
      ZeroCountingDial(position = 20).rotate(Rotation.Left(10)) shouldBe ZeroCountingDial(position = 10, zeroPasses = 0)
      ZeroCountingDial(position = 10).rotate(Rotation.Left(140)) shouldBe ZeroCountingDial(position = 70, zeroPasses = 2)

      ZeroCountingDial(position = 10).rotate(Rotation.Right(0)) shouldBe ZeroCountingDial(position = 10, zeroPasses = 0)
      ZeroCountingDial(position = 10).rotate(Rotation.Left(0)) shouldBe ZeroCountingDial(position = 10, zeroPasses = 0)
      ZeroCountingDial(position = 0).rotate(Rotation.Right(0)) shouldBe ZeroCountingDial(position = 0, zeroPasses = 0)
      ZeroCountingDial(position = 0).rotate(Rotation.Left(0)) shouldBe ZeroCountingDial(position = 0, zeroPasses = 0)
    }

    //    The dial starts by pointing at 50.
    //    The dial is rotated L68 to point at 82; during this rotation, it points at 0 once.
    //    The dial is rotated L30 to point at 52.
    //    The dial is rotated R48 to point at 0.
    //    The dial is rotated L5 to point at 95.
    //    The dial is rotated R60 to point at 55; during this rotation, it points at 0 once.
    //    The dial is rotated L55 to point at 0.
    //    The dial is rotated L1 to point at 99.
    //    The dial is rotated L99 to point at 0.
    //    The dial is rotated R14 to point at 14.
    //    The dial is rotated L82 to point at 32; during this rotation, it points at 0 once.

    it("should work for the example") {
      var dial = ZeroCountingDial(position = 50)

      dial = dial.rotate(Rotation.Left(68))
      dial shouldBe ZeroCountingDial(position = 82, zeroPasses = 1)

      dial = dial.rotate(Rotation.Left(30))
      dial shouldBe ZeroCountingDial(position = 52, zeroPasses = 1)

      dial = dial.rotate(Rotation.Right(48))
      dial shouldBe ZeroCountingDial(position = 0, zeroPasses = 2)

      dial = dial.rotate(Rotation.Left(5))
      dial shouldBe ZeroCountingDial(position = 95, zeroPasses = 2)

      dial = dial.rotate(Rotation.Right(60))
      dial shouldBe ZeroCountingDial(position = 55, zeroPasses = 3)

      dial = dial.rotate(Rotation.Left(55))
      dial shouldBe ZeroCountingDial(position = 0, zeroPasses = 4)

      dial = dial.rotate(Rotation.Left(1))
      dial shouldBe ZeroCountingDial(position = 99, zeroPasses = 4)

      dial = dial.rotate(Rotation.Left(99))
      dial shouldBe ZeroCountingDial(position = 0, zeroPasses = 5)

      dial = dial.rotate(Rotation.Right(14))
      dial shouldBe ZeroCountingDial(position = 14, zeroPasses = 5)

      dial = dial.rotate(Rotation.Left(82))
      dial shouldBe ZeroCountingDial(position = 32, zeroPasses = 6)
    }
  }
}
