package org.algorab.parsing

import io.github.iltotore.pureparser.ParseError
import io.github.iltotore.pureparser.Parser
import io.github.iltotore.pureparser.Span
import io.github.iltotore.pureparser.util.Zip
import org.algorab.AlgorabProgram
import org.algorab.ast.raw.Program
import org.algorab.util.FileName
import org.algorab.util.SourcePosition
import purelogic.*
import scala.annotation.tailrec
import scala.reflect.TypeTest
import scala.compiletime.constValue
import io.github.iltotore.pureparser.RecoverStrategy

/**
 * A program to be evaluated during the name resolution phase.
 * Basically PureParser's [[Parser]] with extra information.
 */
type AlgorabParser[I, +A] = Reader[FileInfo] ?=> Parser[I, A]

object AlgorabParser:

  /**
    * Parse the given textual source.
    *
    * @param file the source file's name
    * @param source the source's content
    * @return the [[Program]] parsed from the source
    */
  def apply(file: FileName, source: String): AlgorabProgram[Program] =
    Reader(FileInfo.fromSource(file, source))(ExprParser(TokenLexer(source)))

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
    * Return both the output of the given [[AlgorabParser]] and the [[SourcePosition]] between the first and last read token.
    *
    * @tparam I the type of a token.
    * @tparam A the output type.
    * @param parser the [[AlgorabParser]] to wrap.
    */
  def position[I, A](parser: AlgorabParser[I, A])(using zip: Zip[A, SourcePosition]): AlgorabParser[I, zip.Zipped] =
    val (result, span) = Parser.span(parser)
    zip.zip(result, toSourcePosition(span))

  /**
   * Expect the given token type for the next token.
   */
  inline def token[A <: Token](using test: TypeTest[Token, A]): AlgorabParser[Token, Unit] =
    Parser.expect(Parser.ofType[Token, A], constValue[Token.ToString[A]])

  /**
   * Like [[Parser.span]], but using [[Token#position]] instead.
   *
   * @param parser the wrapped parser
   * @return the parsed result and the [[SourcePosition]] covering the spans of all parsed tokens
   */
  def tokenPosition[A](parser: AlgorabParser[Token, A])(using zip: Zip[A, SourcePosition]): AlgorabParser[Token, zip.Zipped] =
    val start = get
    val result = parser
    val end = get
    val startPosition = read(_(start).position)
    val endPosition = read(_(math.max(end - 1, 0)).position)
    zip.zip(
      result,
      SourcePosition(
        file = startPosition.file,
        start = startPosition.start,
        end = endPosition.end
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

  def skipUntilPosition[I, A](until: Parser[I, Any], fallback: SourcePosition => A): Reader[FileInfo] ?=> RecoverStrategy[I, A] = new RecoverStrategy:
    override def apply(parser: Parser[I, A]): Parser[I, A] = fallback(position(Parser.skipUntil(parser)))

  /**
    * Convert the given [[SourcePosition]] to a [[Span]].
    *
    * @param position the position to convert
    * @return the [[Span]] representing the same location than the given position in the current file.
    */
  def toSpan(position: SourcePosition): Reader[FileInfo] ?=> Span = read(fileInfo =>
    Span(
      fileInfo.lineSpans(position.start.line).start + position.start.column,
      fileInfo.lineSpans(position.end.line).start + position.end.column
    )
  )

  /**
    * Convert the given [[Span]] to a [[SourcePosition]].
    *
    * @param span the span to convert
    * @return the [[SourcePosition]] representing the same location than the given span in the current file.
    */
  def toSourcePosition(span: Span): Reader[FileInfo] ?=> SourcePosition = read(fileInfo =>
    SourcePosition(
      file = fileInfo.name,
      start = SourcePosition.Point.apply.tupled(fileInfo.lineAndColumn(span.start)),
      end = SourcePosition.Point.apply.tupled(fileInfo.lineAndColumn(span.end))
    )
  )
