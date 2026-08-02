package org.algorab.parsing

import io.github.iltotore.pureparser.*
import org.algorab.AlgorabProgram
import org.algorab.ast.Identifier
import purelogic.*
import scala.annotation.tailrec

object TokenLexer:

  val booleanParser: Parser[Char, Token] = Token.LBool.apply.tupled(
    Parser.span(
      Parser.firstOf(
        Parser.as(Parser.literal("true"), true),
        Parser.as(Parser.literal("false"), false)
      )
    )
  )

  val rawIntParser: Parser[Char, Int] =
    val (intStr, span) = Parser.span(Parser.regex("[0-9]+"))
    intStr.toIntOption.getOrElse(
      Parser.errorAndAbort(ParseError(ParseError.Pattern.Label("Valid Int"), span.start), fatal = true)
    )

  val rawFloatParser: Parser[Char, Double] =
    val (floatStr, span) = Parser.span(Parser.regex(raw"[0-9]+\.[0-9]+"))
    floatStr.toDoubleOption.getOrElse(
      Parser.errorAndAbort(ParseError(ParseError.Pattern.Label("Valid Float"), span.start), fatal = true)
    )

  val exponentParser: Parser[Char, Int] = Parser.expect(
    Parser.regex(raw"(\+|\-)?[0-9]+").toIntOption.getOrElse(Parser.backtrack),
    "Exponent"
  )

  val numberParser: Parser[Char, Token] = Parser.firstOf(
    Token.LFloat.apply.tupled:
      val (mantissa, exponent, span) = Parser.span(
        Parser.inOrder(
          Parser.firstOf(rawFloatParser, rawIntParser.toDouble),
          Parser.unit(Parser.oneOf("eE")),
          Parser.commit(exponentParser)
        )
      )

      (mantissa * math.pow(10, exponent), span)
    ,
    Token.LFloat.apply.tupled(Parser.span(rawFloatParser)),
    Token.LInt.apply.tupled(Parser.span(rawIntParser))
  )

  private val escapeSequences: Map[Char, Char] = Map(
    'n' -> '\n',
    't' -> '\t',
    'r' -> '\r',
    'b' -> '\b',
    'f' -> '\f',
    '"' -> '"',
    '\'' -> '\'',
    '\\' -> '\\'
  )

  private val rawCharParser: Parser[Char, Char] = Parser.firstOf(
    Parser.inOrder(
      Parser.literal('\\'),
      Parser.recoverWith(
        Parser.expect(escapeSequences(Parser.oneOf(escapeSequences.keySet)), "Valid escape sequence after \\"),
        RecoverStrategy.viaParser(Parser.next)
      )
    ),
    Parser.next
  )

  val charParser: Parser[Char, Token] = Token.LChar.apply.tupled(
    Parser.span(
      Parser.inOrder(
        Parser.literal('\''),
        Parser.commit(
          Parser.expect(
            Parser.andCheck(rawCharParser, Parser.not(Parser.literal('\''))),
            "Valid Char between '...'"
          )
        ),
        Parser.commit(Parser.expect(Parser.literal('\''), "`'` to close the char. If you want multiple characters, use a String \"...\" instead."))
      )
    )
  )

  val stringParser: Parser[Char, Token] = Token.LString.apply.tupled(
    Parser.span(
      Parser.inOrder(
        Parser.literal("\""),
        Parser.repeatUntil(
          Parser.commit(Parser.expect(rawCharParser, "character or `\"` to close the String")),
          Parser.literal('"')
        )
          .mkString
          .translateEscapes,
        Parser.literal('\"')
      )
    )
  )

  val literalParser: Parser[Char, Token] = Parser.firstOf(
    booleanParser,
    numberParser,
    charParser,
    stringParser
  )

  private val word: Parser[Char, (String, Span)] = Parser.span(Parser.regex("[a-zA-Z_][a-zA-Z0-9_]*"))

  private val identifierParser: Parser[Char, Token] =
    val (ident, span) = word
    Token.Ident(Identifier.assume(ident), span)

  private val keywords: Map[String, Span => Token] = Map(
    "and" -> Token.And.apply,
    "or" -> Token.Or.apply,
    "not" -> Token.Not.apply,
    "if" -> Token.If.apply,
    "then" -> Token.Then.apply,
    "else" -> Token.Else.apply,
    "for" -> Token.For.apply,
    "while" -> Token.While.apply,
    "do" -> Token.Do.apply,
    "in" -> Token.In.apply,
    "def" -> Token.Def.apply,
    "val" -> Token.Val.apply,
    "mut" -> Token.Mut.apply,
    "package" -> Token.Package.apply
  )

  private val symbols: IndexedSeq[(String, Span => Token)] = Seq(
    "(" -> Token.ParenOpen.apply,
    ")" -> Token.ParenClosed.apply,
    "," -> Token.Comma.apply,
    ":" -> Token.Colon.apply,
    "." -> Token.Dot.apply,
    "+" -> Token.Plus.apply,
    "-" -> Token.Minus.apply,
    "*" -> Token.Mul.apply,
    "/" -> Token.Div.apply,
    "//" -> Token.IntDiv.apply,
    "%" -> Token.Percent.apply,
    "=" -> Token.Equal.apply,
    "==" -> Token.EqualEqual.apply,
    "!=" -> Token.NotEqual.apply,
    "<" -> Token.Less.apply,
    "<=" -> Token.LessEqual.apply,
    ">" -> Token.Greater.apply,
    ">=" -> Token.GreaterEqual.apply
  )
    .sortBy(-_._1.length)
    .toIndexedSeq

  val keywordParser: Parser[Char, Token] =
    val (w, span) = word
    keywords.getOrElse(w, Parser.backtrack)(span)

  val symbolParser: Parser[Char, Token] = Parser.firstOfSeq(
    symbols.map((symbol, token) => token(Parser.span(Parser.literal(symbol))))
  )

  val tokenParser: Parser[Char, Token] = Parser.expect(
    Parser.firstOf(
      literalParser,
      symbolParser,
      keywordParser,
      identifierParser
    ),
    "Token"
  )

  val commentParser: Parser[Char, Unit] = Parser.spaced(
    Parser.unit(
      Parser.firstOf(
        Parser.inOrder(
          Parser.literal("---"),
          Parser.recoverWith(
            Parser.expect(
              Parser.inOrder(Parser.repeatUntil0(Parser.next, Parser.literal("---")), Parser.literal("---")),
              "`---` closing the multiline comment"
            ),
            RecoverStrategy.skipUntil(Parser.eof, ())
          )
        ),
        Parser.inOrder(Parser.literal("--"), Parser.repeatUntil0(Parser.next, Parser.firstOf(Parser.newline, Parser.eof)))
      )
    )
  )

  val tokenListParser: Parser[Char, List[Token]] = Parser.repeatUntil0(
    Parser.recoverWith(
      Parser.inOrder(Parser.repeatDiscard0(commentParser), Parser.spaced(tokenParser), Parser.repeatDiscard0(commentParser)),
      RecoverStrategy.skipThenRetryUntil(Parser.eof)
    ),
    Parser.eof
  )

  private enum LayoutContext derives CanEqual:
    case Layout(column: Int)
    case Parentheses

    def isMoreIndented(column: Int): Boolean = this match
      case Layout(col) => col < column
      case Parentheses => false

    def isMoreIndented(other: LayoutContext): Boolean = other match
      case Layout(column) => this.isMoreIndented(column)
      case Parentheses    => false

    def isLessIndented(column: Int): Boolean = this match
      case Layout(col) => col > column
      case Parentheses => false

    def isAsIndented(column: Int): Boolean = this match
      case Layout(col) => column == col
      case Parentheses => false

  private case class LayoutState(
      stack: List[LayoutContext],
      output: List[Token],
      pendingLayout: Boolean,
      previousPosition: (Int, Int)
  )

  def isLayoutStart(token: Token): Boolean = token match
    case _: (Token.If | Token.Then | Token.Else | Token.For | Token.While | Token.Do | Token.In | Token.Equal) => true
    case _                                                                                                     => false

  def isLayoutEnd(token: Token): Boolean = token match
    case _: (Token.Then | Token.Else | Token.In | Token.Do) => true
    case _                                                  => false

  def indentationParser(tokens: List[Token], source: String): Parser[Char, List[Token]] =
    val lineSpans =
      source
        .split("(\n|\r(?!\n))")
        .scanLeft(Span(0, 0))((spanBefore, line) => Span(spanBefore.end, spanBefore.end + line.length + 1))
        .tail
        .zipWithIndex

    def lineAndColumn(position: Int): (Int, Int) =
      lineSpans
        .collectFirst:
          case (Span(start, end), line) if position < end => (line, position - start)
        .get

    val startPosition = tokens.headOption.fold((0, 0))(token => lineAndColumn(token.span.start))

    val finalState = tokens.foldLeft(LayoutState(List(LayoutContext.Layout(0)), Nil, false, startPosition)): (state, token) =>
      val (line, column) = lineAndColumn(token.span.start)
      val isSameLine = line == state.previousPosition._1

      val withIndent =
        if state.pendingLayout && !isSameLine then
          if !state.stack.head.isMoreIndented(column) then
            write(ParseError(s"Greater indentation than ${state.stack.head}", token.span.start))

          state.copy(
            stack = LayoutContext.Layout(column) :: state.stack,
            output = state.output :+ Token.Indent(Span(lineSpans(line)._1.start, token.span.start))
          )
        else state

      val (dropped, remainingLayouts) = withIndent.stack.span(_.isLessIndented(column))
      val deindents = dropped.map:
        case LayoutContext.Layout(column) => Token.DeIndent(Span(column, column))
        case invalid                      => throw AssertionError(s"Unexpected deindent of non-layout context: $invalid")

      val withDeindents = withIndent.copy(
        stack = remainingLayouts,
        output = withIndent.output ++ deindents
      )

      if withDeindents.stack.head.isMoreIndented(withIndent.stack.head) && withDeindents.stack.head.isMoreIndented(column) then
        write(ParseError(s"Greater or equal indentation than ${state.stack.head}", token.span.start))

      val withNewline =
        if !isSameLine && withDeindents.stack.head.isAsIndented(column) && !withDeindents.pendingLayout && !isLayoutEnd(token) then
          withDeindents.copy(
            output = withDeindents.output :+ Token.Newline(Span(token.span.start, token.span.start))
          )
        else withDeindents

      val withParenHandling = token match
        case Token.ParenOpen(_) => withNewline.copy(stack = LayoutContext.Parentheses :: withNewline.stack)
        case Token.ParenClosed(_) => withNewline.stack match
            case LayoutContext.Parentheses :: tail => withNewline.copy(stack = tail)
            case _                                 => withNewline
        case _ => withNewline

      withParenHandling.copy(
        output = withParenHandling.output :+ token,
        pendingLayout = isLayoutStart(token),
        previousPosition = (line, column)
      )

    finalState.output ++ finalState.stack.init.collect:
      case LayoutContext.Layout(column) => Token.DeIndent(Span(column, column))

  def apply(source: String): AlgorabProgram[List[Token]] =
    val result = Parser(source)(indentationParser(tokenListParser, source))
    Writer.writeAll(result.errors)
    Abort.extractOption(result.output, ())
