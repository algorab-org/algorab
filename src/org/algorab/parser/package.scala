package org.algorab.parser

import io.github.iltotore.pureparser.*
import io.github.iltotore.pureparser.util.Zip
import purelogic.*
import scala.reflect.TypeTest
import scala.annotation.tailrec

def tryParser[I, A](parser: Parser[I, A]): Parser[I, Option[A]] = Parser.firstOf(Some(parser), None)
def tryParserUnit[I](parser: Parser[I, Unit]): Parser[I, Unit] = Parser.firstOf(Parser.unit(parser), ())

def matchingParser[A](f: PartialFunction[Token, A]): Parser[Token, A] =
  f.applyOrElse(Parser.next, _ => Parser.errorAndAbort(ParseError(ParseError.Pattern.SomethingElse, get)))

def tokenTypeParser[A <: Token](using test: TypeTest[Token, A]): Parser[Token, Unit] = matchingParser:
  case test(value) => ()

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

def repeatParser[I, A](parser: Parser[I, A]): Parser[I, List[A]] =

  @tailrec
  def rec(accumulator: List[A]): Parser[I, List[A]] =
    tryParser(parser) match
      case Some(value) => rec(accumulator :+ value)
      case None => accumulator

  rec(Nil)
