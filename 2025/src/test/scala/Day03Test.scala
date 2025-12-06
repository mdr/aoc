import aoc2025.day03.*
import aoc2025.utils.readInput
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Day03Test extends AnyFunSpec with Matchers {

  val exampleInput: String =
    """987654321111111
      |811111111111119
      |234234234234278
      |818181911112111""".stripMargin
  val exampleBanks: Seq[Bank] = Bank.parseBanks(exampleInput)
  val puzzleBanks: Seq[Bank] = Bank.parseBanks(readInput("day03/input.txt"))

  describe("parseBank") {
    it("should parse a single line of digits into a Bank") {
      Bank.parseBank("123") shouldBe Bank(Seq(1, 2, 3))
    }

    it("should handle single digit") {
      Bank.parseBank("5") shouldBe Bank(Seq(5))
    }

    it("should handle zeros") {
      Bank.parseBank("0") shouldBe Bank(Seq(0))
      Bank.parseBank("102") shouldBe Bank(Seq(1, 0, 2))
    }
  }

  describe("parseBanks") {
    it("should parse multiple lines into Banks") {
      val input = """123
                    |456
                    |789""".stripMargin
      Bank.parseBanks(input) shouldBe Seq(
        Bank(Seq(1, 2, 3)),
        Bank(Seq(4, 5, 6)),
        Bank(Seq(7, 8, 9))
      )
    }

    it("should handle single bank") {
      Bank.parseBanks("123") shouldBe Seq(Bank(Seq(1, 2, 3)))
    }
  }

  describe("orderedItemPairs") {
    it("should return all unique pairs where second index is greater than first") {
      orderedItemPairs(Seq(1, 2, 3)) shouldBe Seq((1, 2), (1, 3), (2, 3))
    }

    it("should return empty for empty sequence") {
      orderedItemPairs(Seq.empty[Int]) shouldBe Seq.empty
    }

    it("should return empty for single-element sequence") {
      orderedItemPairs(Seq(1)) shouldBe Seq.empty
    }

    it("should return single pair for two-element sequence") {
      orderedItemPairs(Seq("a", "b")) shouldBe Seq(("a", "b"))
    }
  }

  describe("solvePart1") {
    it("should work on example input") {
      solvePart1(exampleBanks) shouldBe 357
    }
    it("should work on puzzle input") {
      solvePart1(puzzleBanks) shouldBe 17263
    }
  }

  describe("solvePart2") {
    it("should work on example input") {
      solvePart2(exampleBanks) shouldBe 3121910778619L
    }
    it("should work on puzzle input") {
      solvePart2(puzzleBanks) shouldBe 170731717900423L
    }
  }

  describe("allSubsequencesOfLength") {
    it("should return all subsequences of length 2") {
      allSubsequencesOfLength(Seq(1, 2, 3), 2) shouldBe Seq(Seq(1, 2), Seq(1, 3), Seq(2, 3))
    }

    it("should return all subsequences of length 3") {
      allSubsequencesOfLength(Seq(1, 2, 3, 4), 3) shouldBe Seq(
        Seq(1, 2, 3), Seq(1, 2, 4), Seq(1, 3, 4), Seq(2, 3, 4)
      )
    }

    it("should return single empty subsequence for length 0") {
      allSubsequencesOfLength(Seq(1, 2, 3), 0) shouldBe Seq(Seq.empty)
    }

    it("should return empty when length exceeds sequence size") {
      allSubsequencesOfLength(Seq(1, 2), 3) shouldBe Seq.empty
    }

    it("should return single subsequence when length equals sequence size") {
      allSubsequencesOfLength(Seq(1, 2, 3), 3) shouldBe Seq(Seq(1, 2, 3))
    }
  }

}
