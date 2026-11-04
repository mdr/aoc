package aoc.util

import scala.collection.immutable.TreeMap

/** Immutable priority queue: `head` is the greatest element by the ordering.
  *
  * Elements that compare equal are kept in a list under one key, so distinct elements with the same priority are
  * all retained.
  */
class PriorityQueue[A] private (private val elems: TreeMap[A, List[A]], val size: Int):
  def enqueue(elem: A): PriorityQueue[A] =
    new PriorityQueue(elems.updated(elem, elem :: elems.getOrElse(elem, Nil)), size + 1)

  def enqueue(elems: IterableOnce[A]): PriorityQueue[A] = elems.iterator.foldLeft(this)(_.enqueue(_))

  def isEmpty: Boolean = size == 0

  def head: A = elems.last._2.head

  def dequeue: (A, PriorityQueue[A]) =
    val (key, first :: rest) = elems.last: @unchecked
    val remaining = if rest.isEmpty then elems - key else elems.updated(key, rest)
    (first, new PriorityQueue(remaining, size - 1))

  def dequeueOption: Option[(A, PriorityQueue[A])] = Option.when(!isEmpty)(dequeue)

object PriorityQueue:
  def empty[A](using ord: Ordering[A]): PriorityQueue[A] = new PriorityQueue(TreeMap.empty[A, List[A]], 0)

  def apply[A](elems: A*)(using Ordering[A]): PriorityQueue[A] = empty[A].enqueue(elems)
