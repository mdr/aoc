package aoc2025.utils

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class StringUtilsTest extends AnyFunSpec with Matchers {

  describe("splitOnWhitespace") {
    it("should split on spaces") {
      "a b c".splitOnWhitespace shouldBe Seq("a", "b", "c")
    }
    it("should split on multiple spaces") {
      "a  b   c".splitOnWhitespace shouldBe Seq("a", "b", "c")
    }
    it("should split on tabs") {
      "a\tb\tc".splitOnWhitespace shouldBe Seq("a", "b", "c")
    }
    it("should trim leading and trailing whitespace") {
      "  a b c  ".splitOnWhitespace shouldBe Seq("a", "b", "c")
    }
  }

  describe("splitOnBlankLines") {
    it("should split on double newlines") {
      "a\n\nb".splitOnBlankLines shouldBe Seq("a", "b")
    }
    it("should handle multiple sections") {
      "a\nb\n\nc\nd\n\ne".splitOnBlankLines shouldBe Seq("a\nb", "c\nd", "e")
    }
  }

}
