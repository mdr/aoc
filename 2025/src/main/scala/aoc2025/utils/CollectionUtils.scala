package aoc2025.utils

extension [A](xs: Iterable[A])
  def sumBy[B](f: A => B)(using num: Numeric[B]): B =
    xs.foldLeft(num.zero)((acc, x) => num.plus(acc, f(x)))

  def maxOf[B](f: A => B)(using ord: Ordering[B]): B =
    xs.map(f).max

  def maxOfOption[B](f: A => B)(using ord: Ordering[B]): Option[B] =
    xs.map(f).maxOption

extension[A: Numeric](xs: Iterable[A])
  def sumWhere(pred: A => Boolean): A =
    val num = summon[Numeric[A]]
    xs.foldLeft(num.zero)((acc, x) => if pred(x) then num.plus(acc, x) else acc)