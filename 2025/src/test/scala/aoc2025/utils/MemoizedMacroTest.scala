package aoc2025.utils

import aoc2025.utils.cached
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

// Test class for single parameter
class FibCalculator:
  var callCount = 0

  @cached
  def fib(n: Int): Long =
    callCount += 1
    if n <= 1 then n.toLong
    else fib(n - 1) + fib(n - 2)

// Test class for two parameters
class GridPathCalculator:
  var callCount = 0

  @cached
  def pathCount(x: Int, y: Int): Long =
    callCount += 1
    if x == 0 || y == 0 then 1L
    else pathCount(x - 1, y) + pathCount(x, y - 1)

// Test class for three parameters
class ThreeParamCalculator:
  var callCount = 0

  @cached
  def compute(a: Int, b: Int, c: Int): Int =
    callCount += 1
    a + b + c

// Test class for lambdas capturing method parameters
class LambdaCaptureCalculator:
  var callCount = 0
  val items = Seq(1, 2, 3, 4, 5)

  @cached
  def sumWithMultiplier(multiplier: Int, startIndex: Int): Int =
    callCount += 1
    // Lambda captures both method parameters
    startIndex.until(items.length).map { i =>
      items(i) * multiplier
    }.sum

// Test class for nested lambdas capturing parameters
class NestedLambdaCalculator:
  var callCount = 0

  @cached
  def compute(depth: Int, width: Int): Int =
    callCount += 1
    if depth == 0 then width
    else
      // Outer lambda captures depth, inner lambda captures width
      (0 until width).flatMap { i =>
        (0 until depth).map { j =>
          i + j + compute(depth - 1, width)
        }
      }.sum

class CachedMacroTest extends AnyFunSpec with Matchers {

  describe("@cached annotation") {
    it("should cache results of single-parameter fibonacci") {
      val calc = new FibCalculator
      calc.fib(10) shouldBe 55

      // Without caching, fib(10) would call fib() 177 times
      // With caching, it should only call 11 times (n=0 to n=10)
      calc.callCount shouldBe 11
    }

    it("should cache results of two-parameter grid paths") {
      val calc = new GridPathCalculator
      calc.pathCount(5, 5) shouldBe 252

      // With caching, should cache intermediate results
      // Grid is 6x6 points, so at most 36 unique calls
      calc.callCount should be <= 36
    }

    it("should cache results of three-parameter method") {
      val calc = new ThreeParamCalculator

      calc.compute(1, 2, 3) shouldBe 6
      calc.compute(1, 2, 3) shouldBe 6  // cached
      calc.compute(4, 5, 6) shouldBe 15

      // Only 2 unique calls
      calc.callCount shouldBe 2
    }

    it("should handle lambdas that capture method parameters") {
      val calc = new LambdaCaptureCalculator
      // items = Seq(1, 2, 3, 4, 5)
      // sumWithMultiplier(2, 0) = (1+2+3+4+5) * 2 = 30
      calc.sumWithMultiplier(2, 0) shouldBe 30
      // sumWithMultiplier(2, 2) = (3+4+5) * 2 = 24
      calc.sumWithMultiplier(2, 2) shouldBe 24
      // Cached call
      calc.sumWithMultiplier(2, 0) shouldBe 30

      calc.callCount shouldBe 2
    }

    it("should handle nested lambdas capturing parameters") {
      val calc = new NestedLambdaCalculator

      calc.compute(0, 3) shouldBe 3
      calc.compute(0, 3) shouldBe 3  // cached
      calc.callCount shouldBe 1

      // Reset and test deeper recursion
      val calc2 = new NestedLambdaCalculator
      calc2.compute(1, 2) // This exercises nested lambdas with recursion
      val firstCallCount = calc2.callCount
      calc2.compute(1, 2) // Should be cached
      calc2.callCount shouldBe firstCallCount
    }
  }
}
