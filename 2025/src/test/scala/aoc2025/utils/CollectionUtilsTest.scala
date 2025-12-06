package aoc2025.utils

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class CollectionUtilsTest extends AnyFunSpec with Matchers {

  describe("crossProduct") {
    it("should return all pairs from two sets") {
      crossProduct(Set(1, 2), Set("a", "b")) shouldBe Set(
        (1, "a"), (1, "b"),
        (2, "a"), (2, "b")
      )
    }

    it("should return empty set if either input is empty") {
      crossProduct(Set.empty[Int], Set(1, 2)) shouldBe Set.empty
      crossProduct(Set(1, 2), Set.empty[Int]) shouldBe Set.empty
    }
  }

}
