package aoc.util
import scala.util.parsing.combinator.RegexParsers

trait WithParseWithExceptions { self: RegexParsers =>

  protected def parseWithExceptions[T](parser: Parser[T], s: String): T =
    parse(parser, s) match
      case Success(expr, _) => expr
      case Failure(msg, _)  => throw new RuntimeException(msg)
      case Error(msg, _)    => throw new RuntimeException(msg)
}
