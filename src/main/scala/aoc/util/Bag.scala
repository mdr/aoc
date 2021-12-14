package aoc.util

object Bag:
  def of[A](items: Iterable[A]): Bag[A] = Bag(items.countsLong)

  def apply[A](items: (A, Long)*): Bag[A] = items.map { case (a, count) => Bag(Map(a -> count)) }.fold(Bag.empty)(_ ++ _)

  def empty[A] = Bag[A](Map.empty[A, Long])

extension [A](option: Option[A]) def toBag: Bag[A] = option.map(a => Bag(a -> 1L)).getOrElse(Bag.empty)

case class Bag[A](private val _counts: Map[A, Long]):
  def size: Long = _counts.values.sum

  def bagFlatMap[B](f: (A, Long) => Bag[B]): Bag[B] =
    _counts.toSeq.map(f.tupled).fold(Bag.empty)(_ ++ _)

  def ++(that: Bag[A]): Bag[A] = Bag(this._counts.mergeWith(that._counts)(_ + _))

  def transformCounts(f: Long => Long): Bag[A] =
    Bag(_counts.map { case (a, count) => a -> f(count) })

  def apply(a: A): Long = _counts(a)

  def map[B](f: A => B): Bag[B] = bagFlatMap { case (a, count) => Bag(f(a) -> count) }

  def counts: Seq[Long] = _counts.values.toSeq