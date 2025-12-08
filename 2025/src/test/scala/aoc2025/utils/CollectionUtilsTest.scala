package aoc2025.utils

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class CollectionUtilsTest extends AnyFunSpec with Matchers {

  describe("headAndTail") {
    it("should return head and tail") {
      Seq(1, 2, 3, 4).headAndTail shouldBe (1, Seq(2, 3, 4))
    }
    it("should work with single element") {
      Seq(42).headAndTail shouldBe (42, Seq.empty)
    }
  }

  describe("initAndLast") {
    it("should return init and last element") {
      Seq(1, 2, 3, 4).initAndLast shouldBe (Seq(1, 2, 3), 4)
    }
    it("should work with single element") {
      Seq(42).initAndLast shouldBe (Seq.empty, 42)
    }
  }

  describe("everyNth") {
    it("should take every 2nd element") {
      Seq(0, 1, 2, 3, 4, 5).everyNth(2) shouldBe Seq(0, 2, 4)
    }
    it("should take every 3rd element") {
      Seq(0, 1, 2, 3, 4, 5, 6).everyNth(3) shouldBe Seq(0, 3, 6)
    }
    it("should return all elements when n=1") {
      Seq(1, 2, 3).everyNth(1) shouldBe Seq(1, 2, 3)
    }
  }

  describe("indexesOf") {
    it("should return all indexes of an element") {
      Seq('a', 'b', 'a', 'c', 'a').indexesOf('a') shouldBe Seq(0, 2, 4)
    }
    it("should return empty for missing element") {
      Seq(1, 2, 3).indexesOf(5) shouldBe Seq.empty
    }
    it("should work with strings") {
      "abracadabra".indexesOf('a') shouldBe Seq(0, 3, 5, 7, 10)
    }
  }

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

  describe("foldUntil") {
    import ContinueOrStop.*

    it("should fold entire collection when always continuing") {
      val result = Seq(1, 2, 3, 4).foldUntil(0) { (acc, x) => Continue(acc + x) }
      result shouldBe NoMoreElements(10)
    }

    it("should stop early when Stop is returned") {
      val result = Seq(1, 2, 3, 4, 5).foldUntil(0) { (acc, x) =>
        val newAcc = acc + x
        if newAcc >= 6 then Stop(newAcc, x) else Continue(newAcc)
      }
      result shouldBe ConditionMet(6, 3)
    }

    it("should return initial value for empty collection") {
      val result = Seq.empty[Int].foldUntil(42) { (acc, x) => Continue(acc + x) }
      result shouldBe NoMoreElements(42)
    }

    it("should stop on first element if Stop returned immediately") {
      val result = Seq(1, 2, 3).foldUntil(0) { (acc, x) => Stop(acc, x * 10) }
      result shouldBe ConditionMet(0, 10)
    }

    it("should provide access to acc via accessor method") {
      val result = Seq(1, 2, 3).foldUntil(0) { (acc, x) => Continue(acc + x) }
      result.acc shouldBe 6
    }
  }

}
