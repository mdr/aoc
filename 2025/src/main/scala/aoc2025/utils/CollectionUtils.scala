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

sealed trait FoldUntilResult[+Acc, +Elem]:
  def acc: Acc
  def elemOption: Option[Elem]

case class ConditionMet[+Acc, +Elem](acc: Acc, elem: Elem) extends FoldUntilResult[Acc, Elem]:
  def elemOption: Option[Elem] = Some(elem)

case class NoMoreElements[+Acc](acc: Acc) extends FoldUntilResult[Acc, Nothing]:
  def elemOption: Option[Nothing] = None

extension [A](xs: Iterable[A])
  def foldUntil[Acc](initial: Acc)(combine: (Acc, A) => Acc, stopWhen: Acc => Boolean): FoldUntilResult[Acc, A] =
    val iterator = xs.iterator
    var acc = initial
    while iterator.hasNext do
      val elem = iterator.next()
      val newAcc = combine(acc, elem)
      if stopWhen(newAcc) then return ConditionMet(newAcc, elem)
      acc = newAcc
    NoMoreElements(acc)