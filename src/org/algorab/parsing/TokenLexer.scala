package org.algorab.parsing

import io.github.iltotore.pureparser.*
import org.algorab.AlgorabProgram
import org.algorab.ast.Identifier
import org.algorab.util.FileName
import org.algorab.util.SourcePosition
import purelogic.*
import scala.annotation.tailrec
import org.algorab.ast.Symbol.Root.position

/**
 * A [[Token]] parser, also called a lexer.
 */
object TokenLexer:

  val booleanParser: AlgorabParser[Char, Token] = Token.LBool.apply.tupled(
    AlgorabParser.position(
      Parser.firstOf(
        Parser.as(Parser.literal("true"), true),
        Parser.as(Parser.literal("false"), false)
      )
    )
  )

  val rawIntParser: AlgorabParser[Char, Int] =
    val (intStr, span) = Parser.span(Parser.regex("[0-9]+"))
    intStr.toIntOption.getOrElse(
      Parser.errorAndAbort(ParseError(ParseError.Pattern.Label("Valid Int"), span.start), fatal = true)
    )

  val rawFloatParser: AlgorabParser[Char, Double] =
    val (floatStr, span) = Parser.span(Parser.regex(raw"[0-9]+\.[0-9]+"))
    floatStr.toDoubleOption.getOrElse(
      Parser.errorAndAbort(ParseError(ParseError.Pattern.Label("Valid Float"), span.start), fatal = true)
    )

  val exponentParser: AlgorabParser[Char, Int] = Parser.expect(
    Parser.regex(raw"(\+|\-)?[0-9]+").toIntOption.getOrElse(Parser.backtrack),
    "Exponent"
  )

  val numberParser: AlgorabParser[Char, Token] = Parser.firstOf(
    Token.LFloat.apply.tupled:
      val (mantissa, exponent, position) = AlgorabParser.position(
        Parser.inOrder(
          Parser.firstOf(rawFloatParser, rawIntParser.toDouble),
          Parser.unit(Parser.oneOf("eE")),
          Parser.commit(exponentParser)
        )
      )

      (mantissa * math.pow(10, exponent), position)
    ,
    Token.LFloat.apply.tupled(AlgorabParser.position(rawFloatParser)),
    Token.LInt.apply.tupled(AlgorabParser.position(rawIntParser))
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

  private val rawCharParser: AlgorabParser[Char, Char] = Parser.firstOf(
    Parser.inOrder(
      Parser.literal('\\'),
      Parser.recoverWith(
        Parser.expect(escapeSequences(Parser.oneOf(escapeSequences.keySet)), "Valid escape sequence after \\"),
        RecoverStrategy.viaParser(Parser.next)
      )
    ),
    Parser.next
  )

  val charParser: AlgorabParser[Char, Token] = Token.LChar.apply.tupled(
    AlgorabParser.position(
      Parser.inOrder(
        Parser.literal('\''),
        Parser.commit(
          Parser.expect(
            Parser.andCheck(rawCharParser, Parser.not(Parser.literal('\''))),
            "Valid Char between '...'"
          )
        ),
        Parser.recoverWith(
          Parser.expect(
            Parser.literal('\''),
            "Missing `'` to close the char. If you want multiple characters, use a String \"...\" instead."
          ),
          RecoverStrategy.firstOf(
            RecoverStrategy.skipThenRetryUntil(Parser.firstOf(Parser.newline, Parser.eof)),
            RecoverStrategy.skipUntil(Parser.firstOf(Parser.newline, Parser.eof), ())
          )
        )
      )
    )
  )

  val stringParser: AlgorabParser[Char, Token] = Token.LString.apply.tupled(
    AlgorabParser.position(
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

  val literalParser: AlgorabParser[Char, Token] = Parser.firstOf(
    booleanParser,
    numberParser,
    charParser,
    stringParser
  )

  private val word: AlgorabParser[Char, (String, SourcePosition)] = AlgorabParser.position(Parser.regex("[a-zA-Z_][a-zA-Z0-9_]*"))

  private val identifierParser: AlgorabParser[Char, Token] =
    val (ident, position) = word
    Token.Ident(Identifier.assume(ident), position)

  private val keywords: Map[String, SourcePosition => Token] = Map(
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
    "package" -> Token.Package.apply,
    "import" -> Token.Import.apply,
    "as" -> Token.As.apply
  )

  private val symbols: IndexedSeq[(String, SourcePosition => Token)] = Seq(
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

  val keywordParser: AlgorabParser[Char, Token] =
    val (w, position) = word
    keywords.getOrElse(w, Parser.backtrack)(position)

  val symbolParser: AlgorabParser[Char, Token] = Parser.firstOfSeq(
    symbols.map((symbol, token) => token(AlgorabParser.position(Parser.literal(symbol))))
  )

  val tokenParser: AlgorabParser[Char, Token] = Parser.expect(
    Parser.firstOf(
      literalParser,
      symbolParser,
      keywordParser,
      identifierParser
    ),
    "Valid token"
  )

  val commentParser: AlgorabParser[Char, Unit] = Parser.spaced(
    Parser.unit(
      Parser.firstOf(
        Parser.inOrder(
          Parser.literal("---"),
          Parser.recoverWith(
            Parser.expect(
              Parser.inOrder(Parser.repeatUntil(Parser.next, Parser.literal("---")), Parser.literal("---")),
              "`---` closing the multiline comment"
            ),
            RecoverStrategy.skipUntil(Parser.eof, ())
          )
        ),
        Parser.inOrder(Parser.literal("--"), Parser.repeatUntil(Parser.next, Parser.firstOf(Parser.newline, Parser.eof)))
      )
    )
  )

  val commentSurroundedTokenParser: AlgorabParser[Char, Token] = Parser.inOrder(
    Parser.repeatDiscard(commentParser),
    Parser.spaced(
      Parser.firstOf(
        tokenParser,
        Token.Unknown.apply.tupled(AlgorabParser.position(Parser.repeatUntil(Parser.next, Parser.unit(tokenParser)).mkString))
      )
    ),
    Parser.repeatDiscard(commentParser)
  )

  val tokenListParser: AlgorabParser[Char, List[Token]] = Parser.repeatUntil(
    Parser.recoverWith(
      commentSurroundedTokenParser,
      AlgorabParser.skipUntilPosition(Parser.firstOf(commentSurroundedTokenParser, Parser.eof), Token.Invalid.apply)
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
      previousPosition: SourcePosition.Point
  )

  private def isLayoutStart(token: Token): Boolean = token match
    case _: (Token.If | Token.Then | Token.Else | Token.For | Token.While | Token.Do | Token.In | Token.Equal) => true
    case _                                                                                                     => false

  private def isLayoutEnd(token: Token): Boolean = token match
    case _: (Token.Then | Token.Else | Token.In | Token.Do) => true
    case _                                                  => false

  /**
   * Parse indentation and newlines, based on similar layout rules than Haskell's.
   *
   * @param tokens the parsed tokens, excluding indentation-based ones
   * @param source the textual source code, used for getting line and column of a chatacter based on its absolute position
   * @return the token list with [[Token.Indent]]/[[Token.DeIndent]]/[[Token.Newline]] inserted
   */
  def indentationParser(tokens: List[Token], source: String): AlgorabParser[Char, List[Token]] =
    val startPosition = tokens.headOption.fold(SourcePosition.Point(0, 0))(_.position.start)

    val finalState = tokens.foldLeft(LayoutState(List(LayoutContext.Layout(0)), Nil, false, startPosition)): (state, token) =>
      val SourcePosition.Point(line, column) = token.position.start
      val isSameLine = line == state.previousPosition._1

      val withIndent =
        if state.pendingLayout && !isSameLine then
          if !state.stack.head.isMoreIndented(column) then
            write(ParseError(s"Greater indentation than ${state.stack.head}", AlgorabParser.toSpan(token.position).start))

          state.copy(
            stack = LayoutContext.Layout(column) :: state.stack,
            output = state.output :+ Token.Indent(SourcePosition(
              file = read[FileInfo].name,
              start = SourcePosition.Point(line, 0),
              `end` = token.position.start
            ))
          )
        else state

      val (dropped, remainingLayouts) = withIndent.stack.span(_.isLessIndented(column))
      val deindents = dropped.map:
        case LayoutContext.Layout(column) => Token.DeIndent(SourcePosition.at(read[FileInfo].name, line, column))
        case invalid                      => throw AssertionError(s"Unexpected deindent of non-layout context: $invalid")

      val withDeindents = withIndent.copy(
        stack = remainingLayouts,
        output = withIndent.output ++ deindents
      )

      if withDeindents.stack.head.isMoreIndented(withIndent.stack.head) && withDeindents.stack.head.isMoreIndented(column) then
        write(ParseError(s"Greater or equal indentation than ${state.stack.head}", AlgorabParser.toSpan(token.position).start))

      val withNewline =
        if !isSameLine && withDeindents.stack.head.isAsIndented(column) && !withDeindents.pendingLayout && !isLayoutEnd(token) then
          withDeindents.copy(
            output = withDeindents.output :+ Token.Newline(SourcePosition.at(read[FileInfo].name, line, 0))
          )
        else withDeindents

      val withParenHandling = token match
        case Token.ParenOpen(_) => withNewline.copy(stack = LayoutContext.Parentheses :: withNewline.stack)
        case Token.ParenClosed(_) => withNewline.stack match
            case LayoutContext.Parentheses :: tail => withNewline.copy(stack = tail)
            case _                                 => withNewline
        case _ => withNewline

      withParenHandling.copy(
        output = token match
          case Token.Invalid(_) => withParenHandling.output
          case _ => withParenHandling.output :+ token,
        pendingLayout = isLayoutStart(token),
        previousPosition = SourcePosition.Point(line, column)
      )

    finalState.output ++ finalState.stack.init.collect:
      case LayoutContext.Layout(column) => Token.DeIndent(SourcePosition.at(read[FileInfo].name, read[FileInfo].lineSpans.length, 0))

  /**
   * Parse a token list from a textual source code.
   *
   * @param source the source code to read
   * @return the parsed [[Token]]s
   */
  def apply(source: String): Reader[FileInfo] ?=> AlgorabProgram[List[Token]] =
    val result = Parser(source)(indentationParser(tokenListParser, source))
    val info = read[FileInfo]
    Writer.writeAll(result.errors.map(error =>
      val (line, column) = info.lineAndColumn(error.at)
      ParsingError(error.expected, SourcePosition.at(info.name, line, column))
    ))
    Abort.extractOption(result.output, ())
