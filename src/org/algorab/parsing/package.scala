package org.algorab.parsing

import io.github.iltotore.pureparser.*
import io.github.iltotore.pureparser.util.Zip
import org.algorab.AlgorabProgram
import purelogic.*
import scala.annotation.tailrec
import scala.reflect.TypeTest

/**
 * Try the given parser.
 *
 * @param parser the parser to try
 * @return the result wrapped in [[Some]], or [[None]] if it failed
 */
def tryParser[I, A](parser: Parser[I, A]): Parser[I, Option[A]] = Parser.firstOf(Some(parser), None)

/**
 * Match on the next token.
 *
 * @param f the function used to pattern match on the token
 * @return the result of [[f]] applied to the next token
 */
def matchingParser[A](f: PartialFunction[Token, A]): Parser[Token, A] =
  f.applyOrElse(Parser.next, _ => Parser.errorAndAbort(ParseError(ParseError.Pattern.SomethingElse, get)))

/**
 * Expect the given token type for the next token.
 */
def tokenTypeParser[A <: Token](using test: TypeTest[Token, A]): Parser[Token, Unit] = matchingParser:
  case test(value) => ()

/**
 * Like [[Parser.span]], but using [[Token#span]] instead.
 *
 * @param parser the wrapped parser
 * @return the parsed result and the [[Span]] covering the spans of all parsed tokens
 */
def tokenSpan[A](parser: Parser[Token, A])(using zip: Zip[A, Span]): Parser[Token, zip.Zipped] =
  val start = get
  val result = parser
  val end = get
  zip.zip(
    result,
    Span(
      read(_(start).span.start),
      read(_(math.max(end - 1, 0)).span.end)
    )
  )

/**
 * Repeat the given parser until it fails.
 *
 * @param parser the parser to repeat
 * @return all the parsed outputs
 */
def repeatParser[I, A](parser: Parser[I, A]): Parser[I, List[A]] =

  @tailrec
  def rec(accumulator: List[A]): Parser[I, List[A]] =
    tryParser(parser) match
      case Some(value) => rec(accumulator :+ value)
      case None        => accumulator

  rec(Nil)
