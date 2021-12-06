package aoc.util

object Bag:
  def of[A](items: Iterable[A]): Bag[A] = Bag(items.countsLong)

  def apply[A](items: (A, Long)*): Bag[A] = Bag(items.toMap)

  def empty[A] = Bag[A](Map.empty[A, Long])

extension[A](option: Option[A])
  def toBag: Bag[A] = option.map(a => Bag(a -> 1L)).getOrElse(Bag.empty)

case class Bag[A](counts: Map[A, Long]):
  def size: Long = counts.values.sum

  def bagFlatMap[B](f: (A, Long) => Bag[B]): Bag[B] =
    counts.toSeq.map(f.tupled).fold(Bag.empty)(_ ++ _)

  def ++(that: Bag[A]): Bag[A] = Bag(this.counts.mergeWith(that.counts)(_ + _))
