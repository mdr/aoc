import Aoc2024.Day03.Examples
import Aoc2024.Day03.Types
import Std
import Regex
open Regex (Match Captures)

private def instructionRegex: Regex := regex% r"mul\((\d+),(\d+)\)|do\(\)|don't\(\)"

private def matchToMultiplyInstruction {s : String.Slice} (captures: Captures s): Option Instruction :=
  match captures.groups.toList with
  | [some m1, some m2] => Instruction.mul <$> m1.copy.toNat? <*> m2.copy.toNat?
  | _ => none

private def matchToInstruction {s : String.Slice} (captures: Captures s) : Option Instruction :=
  if captures.fullMatch.copy == "do()" then some Instruction.do
  else if captures.fullMatch.copy == "don't()" then some Instruction.dont
  else matchToMultiplyInstruction captures

def findInstructions (s : String) : List Instruction :=
  Regex.all_captures s.toSlice instructionRegex |>.toList |>.flatMap (· |> matchToInstruction |> Option.toList)

#guard findInstructions "mul(1,2) mul(3,4)" == [Instruction.mul 1 2, Instruction.mul 3 4]
#guard findInstructions "do()" == [Instruction.do]
#guard findInstructions "don't()" == [Instruction.dont]
