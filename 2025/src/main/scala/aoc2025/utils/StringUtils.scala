package aoc2025.utils

extension (s: String)
  def splitOnWhitespace: Seq[String] =
    s.trim.split("\\s+").toSeq

  def splitOnBlankLines: Seq[String] =
    s.split("\n\n").toSeq

  def transposeBlock: Seq[String] =
    val lines = s.linesIterator.toSeq
    val maxLen = lines.map(_.length).max
    val paddedLines = lines.map(_.padTo(maxLen, ' '))
    paddedLines.transpose.map(_.mkString)
