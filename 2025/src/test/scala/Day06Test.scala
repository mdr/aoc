import aoc2025.day06.*
import aoc2025.utils.readInput
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Day06Test extends AnyFunSpec with Matchers {

  val exampleInput: String =
    """123 328  51 64
      | 45 64  387 23
      |  6 98  215 314
      |*   +   *   +""".stripMargin
  lazy val puzzleInput: String = readInput("day06/input.txt")

  describe("PuzzleInput.parse") {
    it("should parse number rows and operators") {
      val parsed = PuzzleInput1.parse(exampleInput)
      parsed.numberRows shouldBe Seq(
        Seq(123, 328, 51, 64),
        Seq(45, 64, 387, 23),
        Seq(6, 98, 215, 314)
      )
      parsed.operators shouldBe Seq(Operator.Multiply, Operator.Add, Operator.Multiply, Operator.Add)
    }
  }

  describe("Problem.parse") {
    it("should parse problem 1: 4, 431, 623 with Add") {
      Problem.parse("4\n431\n623+") shouldBe Problem(Seq(4, 431, 623), Operator.Add)
    }
    it("should parse problem 2: 175, 581, 32 with Multiply") {
      Problem.parse("175\n581\n32*") shouldBe Problem(Seq(175, 581, 32), Operator.Multiply)
    }
    it("should parse problem 3: 8, 248, 369 with Add") {
      Problem.parse("8\n248\n369+") shouldBe Problem(Seq(8, 248, 369), Operator.Add)
    }
    it("should parse problem 4: 356, 24, 1 with Multiply") {
      Problem.parse("356\n24\n1  *") shouldBe Problem(Seq(356, 24, 1), Operator.Multiply)
    }
  }

  describe("solvePart1") {
    it("should work on example input") {
      solvePart1(PuzzleInput1.parse(exampleInput)) shouldBe 4277556L
    }
    it("should work on puzzle input") {
      solvePart1(PuzzleInput1.parse(puzzleInput)) shouldBe 4722948564882L
    }
  }

  describe("solvePart2") {
    it("should work on example input") {
      solvePart2(PuzzleInput2.parse(exampleInput)) shouldBe 3263827L
    }
    it("should work on puzzle input") {
      solvePart2(PuzzleInput2.parse(puzzleInput)) shouldBe 9581313737063L
    }
  }

}
