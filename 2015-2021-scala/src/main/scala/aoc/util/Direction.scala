package aoc.util

enum Direction(val x: Int = 0, val y: Int = 0):

  def turnLeft: Direction = this match 
    case Direction.North => Direction.West
    case Direction.South => Direction.East
    case Direction.East => Direction.North
    case Direction.West => Direction.South

  def turnRight: Direction = this match 
    case Direction.North => Direction.East
    case Direction.South => Direction.West
    case Direction.East => Direction.South
    case Direction.West => Direction.North

  case North extends Direction(y = 1)
  case East extends Direction(x = 1)
  case South extends Direction(y = -1)
  case West extends Direction(x = -1)
