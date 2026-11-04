import aoc2025.day10.*
import aoc2025.utils.readInput
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Day10Test extends AnyFunSpec with Matchers {

  val exampleInput: String =
    """[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
      |[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
      |[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}""".stripMargin
  val exampleMachines = Machine.parseMachines(exampleInput)

  val puzzleInput: String = readInput("day10/input.txt")
  val puzzleMachines = Machine.parseMachines(puzzleInput)

  describe("Machine.parseMachines") {
    it("should parse example input into correct number of machines") {
      val machines = Machine.parseMachines(exampleInput)
      machines.size shouldBe 3
    }

    it("should parse first line correctly") {
      // [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
      val machines = Machine.parseMachines(exampleInput)
      val first = machines.head

      first.targetLights shouldBe IndicatorLights(Seq(LightState.Off, LightState.On, LightState.On, LightState.Off))
      first.buttons shouldBe Seq(
        Button(Seq(3)),
        Button(Seq(1, 3)),
        Button(Seq(2)),
        Button(Seq(2, 3)),
        Button(Seq(0, 2)),
        Button(Seq(0, 1))
      )
      first.joltageRequirements shouldBe Joltage(Seq(3, 5, 4, 7))
    }

    it("should parse second line correctly") {
      // [...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
      val machines = Machine.parseMachines(exampleInput)
      val second = machines(1)

      second.targetLights shouldBe IndicatorLights(Seq(
        LightState.Off, LightState.Off, LightState.Off, LightState.On, LightState.Off
      ))
      second.buttons shouldBe Seq(
        Button(Seq(0, 2, 3, 4)),
        Button(Seq(2, 3)),
        Button(Seq(0, 4)),
        Button(Seq(0, 1, 2)),
        Button(Seq(1, 2, 3, 4))
      )
      second.joltageRequirements shouldBe Joltage(Seq(7, 5, 12, 7, 2))
    }

    it("should parse third line correctly") {
      // [.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
      val machines = Machine.parseMachines(exampleInput)
      val third = machines(2)

      third.targetLights shouldBe IndicatorLights(Seq(
        LightState.Off, LightState.On, LightState.On, LightState.On, LightState.Off, LightState.On
      ))
      third.buttons shouldBe Seq(
        Button(Seq(0, 1, 2, 3, 4)),
        Button(Seq(0, 3, 4)),
        Button(Seq(0, 1, 2, 4, 5)),
        Button(Seq(1, 2))
      )
      third.joltageRequirements shouldBe Joltage(Seq(10, 11, 11, 5, 10, 5))
    }
  }

  describe("solvePart1") {
    it("should work on example input") {
      solvePart1(exampleMachines) shouldBe 7
    }
    it("should work on puzzle input") {
      solvePart1(puzzleMachines) shouldBe 542
    }
  }

  describe("solvePart2") {
    it("should work on example input") {
      solvePart2(exampleMachines) shouldBe 33
    }
    ignore("should work on puzzle input") {
      solvePart2(puzzleMachines) shouldBe 0
    }
  }

}
