package org.algorab.parser

import io.github.iltotore.pureparser.*
import purelogic.*
import scala.reflect.TypeTest

def tryParser[I, A](parser: Parser[I, A]): Parser[I, Option[A]] = Parser.firstOf(Some(parser), None)
def tryParserUnit[I](parser: Parser[I, Unit]): Parser[I, Unit] = Parser.firstOf(Parser.unit(parser), ())

def matchingParser[A](f: PartialFunction[Token, A]): Parser[Token, A] =
  f.applyOrElse(Parser.next, _ => Parser.errorAndAbort(ParseError(ParseError.Pattern.SomethingElse, get)))

def tokenTypeParser[A <: Token](using test: TypeTest[Token, A]): Parser[Token, Unit] = matchingParser:
  case test(value) => ()