package org.algorab.parser

import io.github.iltotore.pureparser.*
import purelogic.*
import org.algorab.ast.Identifier

type TokenLexer = Parser[Char, Token]
object TokenLexer:

  val booleanParser: TokenLexer = Token.LBool.apply.tupled(
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

  val numberParser: TokenLexer = Parser.firstOf(
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

  val charParser: TokenLexer = Token.LChar.apply.tupled(
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

  val stringParser: TokenLexer = Token.LString.apply.tupled(
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

  val literalParser: TokenLexer = Parser.firstOf(
    booleanParser,
    numberParser,
    charParser,
    stringParser
  )

  private val word: Parser[Char, (String, Span)] = Parser.span(Parser.regex("[a-zA-Z][a-zA-Z0-9]*"))

  private val identifierParser: TokenLexer =
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
  )

  private val symbols: IndexedSeq[(String, Span => Token)] = Seq(
    "(" -> Token.ParenOpen.apply,
    ")" -> Token.ParenClosed.apply,
    "," -> Token.Comma.apply,
    ":" -> Token.Colon.apply,
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

  val keywordParser: TokenLexer =
    val (w, span) = word
    keywords.getOrElse(w, Parser.backtrack)(span)

  val symbolParser: TokenLexer = Parser.firstOfSeq(
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

  val lexer: Parser[Char, List[Token]] = Parser.repeatUntil0(
    Parser.recoverWith(
      Parser.inOrder(tryParserUnit(commentParser), Parser.spaced(tokenParser), tryParserUnit(commentParser)),
      RecoverStrategy.skipThenRetryUntil(Parser.eof)
    ),
    Parser.eof
  )