package aoc2025.utils

extension [A](xs: Seq[A])
  def headAndTail: (A, Seq[A]) = (xs.head, xs.tail)

  def initAndLast: (Seq[A], A) = (xs.init, xs.last)

  def everyNth(n: Int): Seq[A] = xs.zipWithIndex.collect { case (x, i) if i % n == 0 => x }

  def indexesOf(elem: A): Seq[Int] = xs.zipWithIndex.collect { case (x, i) if x == elem => i }

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

def iterateUntilStable[T](initial: T)(f: T => T): T =
  Iterator.iterate(initial)(f).sliding(2).find { case Seq(a, b) => a == b }.map(_.head).get

def crossProduct[A, B](as: Set[A], bs: Set[B]): Set[(A, B)] =
  for
    a <- as
    b <- bs
  yield (a, b)