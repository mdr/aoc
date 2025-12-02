package aoc2025.utils

extension [A](xs: Iterable[A])
  def sumBy[B](f: A => B)(using num: Numeric[B]): B =
    xs.foldLeft(num.zero)((acc, x) => num.plus(acc, f(x)))

extension[A: Numeric](xs: Iterable[A])
  def sumWhere(pred: A => Boolean): A =
    val num = summon[Numeric[A]]
    xs.foldLeft(num.zero)((acc, x) => if pred(x) then num.plus(acc, x) else acc)