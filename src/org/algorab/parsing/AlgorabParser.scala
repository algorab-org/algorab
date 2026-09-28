package org.algorab.parsing

import io.github.iltotore.pureparser.ParseError
import io.github.iltotore.pureparser.Parser
import io.github.iltotore.pureparser.Span
import io.github.iltotore.pureparser.util.Zip
import org.algorab.util.FileName
import purelogic.*
import scala.annotation.tailrec
import scala.reflect.TypeTest
import org.algorab.AlgorabProgram
import org.algorab.ast.raw.Program

type AlgorabParser[I, +A] = Reader[FileName] ?=> Parser[I, A]

object AlgorabParser:

  def apply(file: FileName, source: String): AlgorabProgram[Program] = Reader(file)(ExprParser(TokenLexer(source)))

  /**
   * Try the given parser.
   *
   * @param parser the parser to try
   * @return the result wrapped in [[Some]], or [[None]] if it failed
   */
  def option[I, A](parser: AlgorabParser[I, A]): AlgorabParser[I, Option[A]] = Parser.firstOf(Some(parser), None)

  /**
   * Match on the next token.
   *
   * @param f the function used to pattern match on the token
   * @return the result of [[f]] applied to the next token
   */
  def matching[A](f: PartialFunction[Token, A]): AlgorabParser[Token, A] =
    f.applyOrElse(Parser.next, _ => Parser.errorAndAbort(ParseError(ParseError.Pattern.SomethingElse, get)))

  /**
   * Expect the given token type for the next token.
   */
  def token[A <: Token](using test: TypeTest[Token, A]): AlgorabParser[Token, Unit] = matching:
    case test(value) => ()

  /**
   * Like [[Parser.span]], but using [[Token#span]] instead.
   *
   * @param parser the wrapped parser
   * @return the parsed result and the [[Span]] covering the spans of all parsed tokens
   */
  def tokenPosition[A](parser: AlgorabParser[Token, A])(using zip: Zip[A, Span]): AlgorabParser[Token, zip.Zipped] =
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
  def repeat[I, A](parser: AlgorabParser[I, A]): AlgorabParser[I, List[A]] =

    @tailrec
    def rec(accumulator: List[A]): AlgorabParser[I, List[A]] =
      option(parser) match
        case Some(value) => rec(accumulator :+ value)
        case None        => accumulator

    rec(Nil)

  /**
   * Apply a function on the parser's output.
   *
   * Strictly the same as `f(parser)` but sometimes this notation plays better than direct-style.
   *
   * @param parser the parser to map
   * @param f the mapping function
   * @return a parser behaving the same as the original parser with `f` applied to its result
   */
  def map[I, A, B](parser: AlgorabParser[I, A])(f: A => B): AlgorabParser[I, B] = f(parser)
