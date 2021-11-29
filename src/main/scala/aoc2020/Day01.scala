package aoc2020

import aoc.util._

val entries = loadLines("aoc2020/input-2020-1.txt").map(_.toInt)

def day1Part1: Int =
  val matchingEntries = findEntriesThatSum(entries, numberOfEntries = 2, targetSum = 2020).single
  matchingEntries.product

def day1Part2: Int =
  val matchingEntries = findEntriesThatSum(entries, numberOfEntries = 3, targetSum = 2020).single
  matchingEntries.product

def findEntriesThatSum(entries: Seq[Int], numberOfEntries: Int, targetSum: Int): Seq[EntryGroup] =
  entries
    .combinations(numberOfEntries)
    .map(entries => EntryGroup(entries.sorted))
    .filter(_.sum == targetSum)
    .toSeq

case class EntryGroup(entries: Seq[Int]):
  val product: Int = entries.product
  val sum: Int = entries.sum