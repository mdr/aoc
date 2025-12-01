package aoc2025

import scala.io.Source

def readInput(path: String): String =
  Source.fromResource(path).mkString
