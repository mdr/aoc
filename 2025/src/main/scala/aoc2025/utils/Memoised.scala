package aoc2025.utils

class Memoised[Key, Result](compute: (Key, Key => Result) => Result):
  private var cache: Map[Key, Result] = Map.empty

  def apply(key: Key): Result =
    cache.getOrElse(key, {
      val result = compute(key, apply)
      cache = cache.updated(key, result)
      result
    })
