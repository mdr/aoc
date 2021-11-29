package aoc.util

enum Direction(val x: Int = 0, val y: Int = 0):
  case North extends Direction(y = 1)
  case East extends Direction(x = 1)
  case South extends Direction(y = -1)
  case West extends Direction(x = -1)
