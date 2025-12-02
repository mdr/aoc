import aoc2025.day02.*
import aoc2025.readInput
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Day02Test extends AnyFunSpec with Matchers {

  val exampleInput = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"
  val exampleRanges: Seq[Range] = Range.parseRanges(exampleInput)
  val puzzleRanges: Seq[Range] = Range.parseRanges(readInput("day02/input.txt"))

  describe("sumOfInvalidIds - duplicated") {
    it("should work on example input") {
      sumOfInvalidIds(exampleRanges, Invalidity.Duplicated) shouldBe 1227775554L
    }
    it("should work on puzzle input") {
      sumOfInvalidIds(puzzleRanges, Invalidity.Duplicated) shouldBe 16793817782L
    }
  }

  describe("sumOfInvalidIds - replicated") {
    it("should work on example input") {
      sumOfInvalidIds(exampleRanges, Invalidity.Replicated) shouldBe 4174379265L
    }
    it("should work on puzzle input") {
      sumOfInvalidIds(puzzleRanges, Invalidity.Replicated) shouldBe 27469417404L
    }
  }

}
