package aoc2021

import aoc.util.*

import scala.PartialFunction.condOpt

val lines: Seq[Line] = loadLines("aoc2021/input-2021-10.txt").map(Line(_))

def day10Part1: Int = solveDay1(lines)

def day10Part2: Long = solveDay2(lines)

def solveDay1(lines: Seq[Line]) = lines.flatMap(_.findIllegalCharacter).sumBy(score)

def solveDay2(lines: Seq[Line]): Long =
  val scores = lines.filterNot(_.isCorrupted).map(_.completeString).map(score).sorted
  scores(scores.length / 2)

def score(c: Char): Int =
  c match
    case ')' => 3
    case ']' => 57
    case '}' => 1197
    case '>' => 25137

val BracketMap = Map(
  '(' -> ')',
  '[' -> ']',
  '{' -> '}',
  '<' -> '>'
)

def score(completionString: List[Char]): Long =
  completionString.foldLeft(0L) { case (score, c) =>
    val characterValue = c match
      case ')' => 1
      case ']' => 2
      case '}' => 3
      case '>' => 4
    score * 5 + characterValue
  }

sealed trait SyntaxAnalysisResult
case class IllegalCharacter(c: Char)            extends SyntaxAnalysisResult
case class Incomplete(missingChars: List[Char]) extends SyntaxAnalysisResult

case class Line(s: String):

  def isCorrupted: Boolean = findIllegalCharacter.isDefined

  def completeString: List[Char] =
    analyseSyntax(List.empty, 0) match
      case Incomplete(cs) => cs
      case c              => throw RuntimeException(s"Unexpected corrupted string: $c")

  def findIllegalCharacter: Option[Char] =
    condOpt(analyseSyntax(List.empty, 0)) { case IllegalCharacter(c) => c }

  private def analyseSyntax(bracketStack: List[Char], position: Int): SyntaxAnalysisResult =
    if position >= s.length then Incomplete(bracketStack)
    else
      val c = s(position)
      BracketMap.get(c) match
        case Some(closingChar) =>
          analyseSyntax(closingChar :: bracketStack, position + 1)
        case None =>
          bracketStack match
            case `c` :: rest => analyseSyntax(rest, position + 1)
            case _           => IllegalCharacter(c)
