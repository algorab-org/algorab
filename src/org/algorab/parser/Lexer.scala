package org.algorab.parser

import io.github.iltotore.pureparser.*
import purelogic.*

type TokenLexer = Parser[Char, Token]
object TokenLexer:

  //TODO: better escape translation to produce good error messages in case of invalide escapes

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

  val charParser: TokenLexer = Token.LChar.apply.tupled(
    Parser.span(
      Parser.inOrder(
        Parser.literal('\''),
        Parser.commit(
          Parser.expect(
            Parser.firstOf(
              Parser.regex(raw"\\.").translateEscapes.charAt(0),
              Parser.andCheck(Parser.next, Parser.not(Parser.literal('\'')))
            ),
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
          Parser.commit(Parser.expect(Parser.next, "character or `\"` to close the String")),
          Parser.andCheck(Parser.literal('"'), localState(_ - 1)(Parser.not(Parser.literal('\\'))))
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
