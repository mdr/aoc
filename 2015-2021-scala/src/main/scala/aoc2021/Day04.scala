package aoc2021

import aoc.util.*
import scala.annotation.tailrec

val bingo = parseBingo(loadString("aoc2021/input-2021-4.txt"))

def day04Part1: Int = bingo.play()

def day04Part2: Int = bingo.playToLose()

case class Bingo(numbers: Seq[Int], boards: Seq[Board]):

  @tailrec
  final def play(nextNumberIndex: Int = 1): Int =
    val drawnNumbers = numbers.take(nextNumberIndex + 1).toSet
    boards.find(_.hasWon(drawnNumbers)) match
      case None =>
        play(nextNumberIndex + 1)
      case Some(board) =>
        board.sumOfAllUnmarkedNumbers(drawnNumbers) * numbers(nextNumberIndex)

  @tailrec
  final def playToLose(nextNumberIndex: Int = 1, unwonBoards: Set[Board] = boards.toSet): Int =
    val drawnNumbers  = numbers.take(nextNumberIndex + 1).toSet
    val winningBoards = unwonBoards.filter(_.hasWon(drawnNumbers))
    if unwonBoards.size == 1 && winningBoards.size == 1 then
      val lastBoard = winningBoards.head
      lastBoard.sumOfAllUnmarkedNumbers(drawnNumbers) * numbers(nextNumberIndex)
    else playToLose(nextNumberIndex + 1, unwonBoards diff winningBoards)

case class Board(rows: Seq[Seq[Int]]):

  private def columns: Seq[Seq[Int]] = rows.transpose

  def hasWon(drawnNumbers: Set[Int]): Boolean =
    rows.exists(_.toSet subsetOf drawnNumbers) || columns.exists(_.toSet subsetOf drawnNumbers)

  def sumOfAllUnmarkedNumbers(drawnNumbers: Set[Int]): Int =
    (rows.flatten.toSet diff drawnNumbers).sum

def parseBingo(s: String): Bingo =
  val allLines                       = getLines(s)
  val numbers                        = allLines.head.split(",").toList.map(_.toInt)
  def parseRow(s: String): Seq[Int]  = s.trim.split("\\s+").map(_.toInt).toSeq
  def parseBoard(lines: Seq[String]) = Board(lines.tail.map(parseRow))
  val boards                         = allLines.tail.grouped(6).map(parseBoard).toSeq
  Bingo(numbers, boards)
