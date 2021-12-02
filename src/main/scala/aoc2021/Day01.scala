package aoc2021

import aoc.util.*

val depths = loadLines("aoc2021/input-2021-1.txt").map(_.toInt)

def day01Part1: Int = countIncreases(depths)

def day01Part2: Int = countSlidingWindowIncreases(depths)

def countIncreases(numbers: Seq[Int]): Int = numbers.drop(1).zip(numbers).count(_ > _)

def countSlidingWindowIncreases(numbers: Seq[Int]): Int = countIncreases(numbers.sliding(3).toSeq.map(_.sum))
