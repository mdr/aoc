package aoc.util
import scala.util.parsing.combinator.RegexParsers

trait DefaultParser extends RegexParsers {

  protected def parseWithExceptions[T](parser: Parser[T], s: String): T =
    parse(parser, s) match
      case Success(expr, _) => expr
      case Failure(msg, _)  => throw new RuntimeException(s"Parse error: $msg\n$s")
      case Error(msg, _)    => throw new RuntimeException("Parse error: $msg\n$s")

  protected lazy val integer: Parser[Int] = """(0|[1-9]\d*)""".r ^^ (_.toInt)

}
