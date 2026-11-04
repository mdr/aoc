package aoc2021

import aoc.util.*

val entries: Seq[Entry] = loadLines("aoc2021/input-2021-8.txt").map(EntryParser.apply)

def day08Part1: Int = countEasyDigits(entries)

def day08Part2: Int = solve(entries)

val StandardWiring = Seq(
  "abcefg",  // 0
  "cf",      // 1
  "acdeg",   // 2
  "acdfg",   // 3
  "bcdf",    // 4
  "abdfg",   // 5
  "abdefg",  // 6
  "acf",     // 7
  "abcdefg", // 8
  "abcdfg"   // 9
)

val StandardWiringLookup: Map[Set[Char], Int] =
  StandardWiring.zipWithIndex.map { case (s, i) => s.toSet -> i }.toMap

case class Mapping(chars: String) {
  def decode(signalPattern: String): String = signalPattern.toSeq.map(c => chars(c - 'a')).mkString

  def decode(signalPattern: SignalPattern): String = decode(signalPattern.wires)

  def tryDecode(signalPattern: SignalPattern): Option[Int] = StandardWiringLookup.get(decode(signalPattern).toSet)

  def solves(patterns: Seq[SignalPattern]): Boolean =
    patterns.flatMap(tryDecode).toSet == (0 to 9).toSet
}

def solve(entries: Seq[Entry]): Int = entries.sumBy(solveEntry)

def solveEntry(entry: Entry) =
  val options = "abcdefg".permutations.map(Mapping(_))
  val mapping = options.find(_.solves(entry.signalPatterns)).get
  entry.outputDigits.flatMap(mapping.tryDecode).mkString.toInt

val EasyLengths = Seq(1, 4, 7, 8).map(n => StandardWiring(n).length)

case class SignalPattern(wires: String) {
  def isEasyDigit: Boolean = EasyLengths.contains(wires.length)
  def length               = wires.length
  def chars: Seq[Char]     = wires.toSeq
}

case class Entry(signalPatterns: Seq[SignalPattern], outputDigits: Seq[SignalPattern])

object EntryParser extends DefaultParser:
  def apply(s: String): Entry = parseWithExceptions(entry, s)
  lazy val entry: Parser[Entry] = repN(10, signalPattern) ~ ("|" ~> repN(4, signalPattern)) ^^ {
    case signalPatterns ~ outputDigits => Entry(signalPatterns, outputDigits)
  }
  lazy val signalPattern = regex("[a-g]+".r) ^^ SignalPattern.apply

def countEasyDigits(entries: Seq[Entry]): Int =
  entries.flatMap(_.outputDigits).count(_.isEasyDigit)

val AllWires: Seq[Char] = "abcdefg".toSeq

object SolveState {

  def initial(entry: Entry) =
    val wireSolveState = OneToOneSolveState(AllWires.map(wire => wire -> AllWires.toSet).toMap)
    val digitSolveState = OneToOneSolveState(
      StandardWiring.map(digit => digit.toSet -> entry.signalPatterns.map(_.wires.toSet).toSet).toMap
    )
    SolveState(wireSolveState, digitSolveState)

}

case class SolveState(
    wireSolveState: OneToOneSolveState[Char, Char],
    digitSolveState: OneToOneSolveState[Set[Char], Set[Char]]
):

  def constrainDigitsByLength: SolveState =
    val newDigitSolveState = OneToOneSolveState(digitSolveState.options.map { case (digit, options) =>
      digit -> options.filter(_.size == digit.size)
    })
    copy(digitSolveState = newDigitSolveState)

  def propagateDigitOptionsToWires: SolveState =
    digitSolveState.options.foldLeft(this) { case (state, (digit, digitOptions)) =>
      state.propagateDigitOptionsToWires(digit, digitOptions)
    }

  def propagateDigitOptionsToWires(digit: Set[Char], digitOptions: Set[Set[Char]]): SolveState =
    val possibleChars = digitOptions.flatten
    val newWireSolveState =
      digit.foldLeft(wireSolveState) { case (state, wire) => state.constrainToBeOneOf(wire, possibleChars) }
    copy(wireSolveState = newWireSolveState)

  def applyAllDifferentConstraint: SolveState =
    copy(
      wireSolveState = wireSolveState.applyAllDifferentConstraint,
      digitSolveState = digitSolveState.applyAllDifferentConstraint
    )

  def applyDigitSubsetConstraints: SolveState =
    digitSolveState.determined.toSeq.foldLeft(this) { case (state, (digit, option)) =>
      state.applyDigitSubsetConstraints(digit, option)
    }

  def applyDigitSubsetConstraints(digit: Set[Char], option: Set[Char]): SolveState =
    val newDigitSolveState = OneToOneSolveState(digitSolveState.options.map { case (d, options) =>
      d -> (if digit subsetOf d then options.filter(option.subsetOf) else options)
    })
    copy(digitSolveState = newDigitSolveState)

  override def toString: String = 
      val wss = wireSolveState.options.keys.toSeq.sorted.map(a => s"$a: ${wireSolveState.options(a).toSeq.sorted.mkString("")}").mkString(", ")
      val dss = digitSolveState.options.keys.toSeq.sorted.map(a => s"${a.mkString}: [${digitSolveState.options(a).toSeq.map(_.mkString).sorted.mkString(", ")}]").mkString(", ")        
      s"SolveState:\n  ${wss}\n  ${dss}"

case class OneToOneSolveState[A, B](options: Map[A, Set[B]]):

  def isInconsistent: Boolean = options.values.exists(_.isEmpty)

  def determined: Map[A, B] = options.collect { case (a, bs) if bs.size == 1 => a -> bs.head }

  def applyAllDifferentConstraint: OneToOneSolveState[A, B] =
    options.invert.toSeq
      .collect { case (bs, as) if as.size == bs.size => as.toSet -> bs }
      .foldLeft(this) { case (state, (as, bs)) => state.knockout(as, bs) }

  private def knockout(exceptAs: Set[A], bsToKnockOut: Set[B]): OneToOneSolveState[A, B] =
    val newOptions =
      options.map { case (a, bs) =>
        val newBs = if exceptAs contains a then bs else bs diff bsToKnockOut
        a -> newBs
      }
    OneToOneSolveState(newOptions)

  def constrainToBeOneOf(a: A, bs: Set[B]): OneToOneSolveState[A, B] =
    setOptions(a, bs intersect options(a))

  private def setOptions(a: A, bs: Set[B]): OneToOneSolveState[A, B] =
    OneToOneSolveState[A, B](options + (a -> bs))

  override def toString: String =
    def renderOptions(a: A) = options(a).mkString(", ")
    options.keys.map(a => s"$a: [${renderOptions(a)}]").mkString(", ")
