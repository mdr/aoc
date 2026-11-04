package aoc2025.utils

import scala.io.Source

def readInput(path: String): String =
  Source.fromResource(path).mkString
