import Aoc2024.Day04.Examples
import Aoc2024.Day04.Types
import Aoc2024.Utils
import Std

def parseGrid (s : String) : Grid := s.splitLines.map (·.toList)
