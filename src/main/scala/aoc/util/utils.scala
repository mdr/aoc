package aoc.util

import scala.annotation.tailrec
import scala.io.Source
import scala.math.Numeric.Implicits.*
import scala.collection.mutable.ArrayBuffer

def loadString(path: String): String = Source.fromResource(path).mkString

def loadLines(path: String): Seq[String] =
  Source.fromResource(path).getLines().toSeq

extension [T](items: Seq[T])
  def traverse[U](f: T => Seq[U]): Seq[Seq[U]] = items.map(f).sequence

  def single: T =
    if items.size == 1 then items(0)
    else throw new AssertionError(s"Expecting exactly 1 element, but were ${items.size}: $items")

extension [T](xs: Seq[Seq[T]])
  def sequence: Seq[Seq[T]] =
    xs match
      case Seq() => Seq(Seq())
      case Seq(first, rest @ _*) =>
        for
          x  <- first
          xs <- rest.sequence
        yield x +: xs

extension [A](items: IterableOnce[A])
  def sumBy[B](f: A => B)(using numeric: Numeric[B]): B =
    val iterator = items.iterator
    if (!iterator.hasNext)
      return numeric.zero
    var result = f(iterator.next())
    while (iterator.hasNext)
      result = result + f(iterator.next())
    result

@tailrec
def iterate[T](value: T, times: Int)(f: T => T): T =
  if (times == 0) value else iterate(f(value), times - 1)(f)

@tailrec
def iterateUntilSteadyState[T](current: T)(f: T => T): T =
  val next = f(current)
  if (next == current) current else iterateUntilSteadyState(next)(f)

def uninterleave[T](items: Seq[T]): (Seq[T], Seq[T]) =
  val items1 = new ArrayBuffer[T]()
  val items2 = new ArrayBuffer[T]()
  for 
    (item, i) <- items.zipWithIndex
    targetItems = if i % 2 == 0 then items1 else items2
  do
    targetItems.addOne(item)
  (items1.toSeq, items2.toSeq)

class MemoContext[K, V](f: K => V):
  private var calculated: Map[K, V] = Map()

  def apply(k: K) = calculated.get(k) getOrElse calculate(k)

  private def calculate(k: K): V =
    val v = f(k)
    calculated = calculated + (k -> v)
    v

def memo[K, V](f: K => V): K => V = new MemoContext(f).apply