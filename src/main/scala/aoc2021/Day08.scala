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

  val Initial = SolveState(AllWires.map(wire => wire -> AllWires.toSet).toMap)

}

case class SolveState(signalWireToSegmentOptions: Map[Char, Set[Char]]):

  def isInconsistent: Boolean =
    val definites = signalWireToSegmentOptions.toSeq.collect {
      case (wire, options) if options.size == 1 => options.head -> wire
    }
    val xyz = definites.groupBy(_._1)
    signalWireToSegmentOptions.values.exists(_.isEmpty) ||
    definites.toSeq.groupBy(_._1).exists(_._2.size > 1)

  override def toString: String =
    AllWires.map(wire => s"$wire: ${signalWireToSegmentOptions(wire).toSeq.sorted.mkString}").mkString(", ")

  def pruneOnLengths(signalPatterns: Seq[SignalPattern]): SolveState =
    EasyLengths.foldLeft(this) { case (state, length) => state.pruneOnLength(signalPatterns, length) }

  private def pruneOnLength(signalPatterns: Seq[SignalPattern], easyLength: Int): SolveState =
    val segmentOptions = StandardWiring.find(_.length == easyLength).get.toSet
    val wires          = signalPatterns.find(_.length == easyLength).toSeq.flatMap(_.chars)
    wires.foldLeft(this) { case (state, wire) => state.restrict(wire, segmentOptions) }

  def restrict(wire: Char, options: Set[Char]): SolveState =
    val newOptions = signalWireToSegmentOptions(wire) intersect options
    SolveState(signalWireToSegmentOptions + (wire -> newOptions))

  def solve: Option[Map[Char, Char]] =
    println(s"solve $this")
    if isInconsistent then None
    else getSolution orElse startGuessing

  private def startGuessing: Option[Map[Char, Char]] =
    println(s"startGuessing $this")
    val solutions =
      for {
        (wire, options) <- signalWireToSegmentOptions.to(LazyList)
        if options.size > 1
        option <- options
        updatedState = guess(wire, option)
        solution <- updatedState.solve
      } yield solution
    solutions.headOption

  private def guess(signalWire: Char, segment: Char): SolveState =
    val state = SolveState(signalWireToSegmentOptions.map { case (wire, options) =>
      wire -> (if wire == signalWire then Set(segment) else options - segment)
    })
    println(s"Guessing $signalWire is $segment:")
    println(this)
    println(state)
    state

  private def getSolution: Option[Map[Char, Char]] =
    if signalWireToSegmentOptions.values.forall(_.size == 1) then
      println(s"Found solution in state: $this")
      val solution = signalWireToSegmentOptions.map { case (wire, options) => wire -> options.head }.toMap
      println(solution)
      println(s"isInconsistent = $isInconsistent")
      val definites = signalWireToSegmentOptions.toSeq.collect {
        case (wire, options) if options.size == 1 => options.head -> wire
      }
      val xyz = definites.toSeq.groupBy(_._1)
      println(definites)
      println(xyz)
      Some(solution)
    else None

case class OneToOneSolveState[A, B](options: Map[A, Set[B]]):

  def isInconsistent: Boolean = options.values.exists(_.isEmpty)

  def applyAllDifferentConstraint: OneToOneSolveState[A, B] =
    val determined: Map[A, B] = options.collect { case (a, bs) if bs.size == 1 => a -> bs.head }
    val determinedBs: Set[B]  = determined.values.toSet
    val newOptions =
      options.map { case (a, bs) =>
        val newBs = if determined contains a then bs else bs diff determinedBs
        a -> newBs
      }
    OneToOneSolveState(newOptions)

  def constrainToBeOneOf(a: A, bs: Set[B]): OneToOneSolveState[A, B] =
    setOptions(a, bs intersect options(a))

  private def setOptions(a: A, bs: Set[B]): OneToOneSolveState[A, B] =
    OneToOneSolveState[A, B](options + (a -> bs))

  override def toString: String =
    def renderOptions(a: A): String = options(a).mkString(", ")
    options.keys.map(a => s"$a: [${renderOptions(a)}]").mkString(",")
