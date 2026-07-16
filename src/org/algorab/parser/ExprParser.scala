package org.algorab.parser

import io.github.iltotore.pureparser.*
import org.algorab.ast.Expr

object ExprParser:

  val literalParser: Parser[Token, Expr] = Parser.next match
    case Token.LBool(value, span)      => Expr.LBool(value, span)
    case Token.LInt(value, span)       => Expr.LInt(value, span)
    case Token.LFloat(value, span)     => Expr.LFloat(value, span)
    case Token.LChar(value, span)      => Expr.LChar(value, span)
    case Token.LString(value, span)    => Expr.LString(value, span)
    case Token.Ident(identifier, span) => Expr.VarCall(identifier, span)
    case _                             => Parser.backtrack

  val termParser: Parser[Token, Expr] = Parser.firstOf(
    literalParser,
    Parser.inOrder(tokenTypeParser[Token.ParenOpen], exprParser, tokenTypeParser[Token.ParenClosed])
  )

  private val prefixOps: PartialFunction[Token, (Expr, Span) => Expr] =
    case Token.Not(_)   => Expr.Not.apply
    case Token.Minus(_) => Expr.Minus.apply
    case Token.Plus(_)  => Expr.Plus.apply

  private val binaryMulOps: PartialFunction[Token, (Expr, Expr, Span) => Expr] =
    case Token.Mul(_)     => Expr.Mul.apply
    case Token.Div(_)     => Expr.Div.apply
    case Token.Percent(_) => Expr.Mod.apply

  private val binaryAddOps: PartialFunction[Token, (Expr, Expr, Span) => Expr] =
    case Token.Plus(_)  => Expr.Add.apply
    case Token.Minus(_) => Expr.Sub.apply

  private val binaryCompOps: PartialFunction[Token, (Expr, Expr, Span) => Expr] =
    case Token.EqualEqual(_)   => Expr.Equal.apply
    case Token.Greater(_)      => Expr.Greater.apply
    case Token.GreaterEqual(_) => Expr.GreaterEqual.apply
    case Token.Less(_)         => Expr.Less.apply
    case Token.LessEqual(_)    => Expr.LessEqual.apply

  private val binaryBoolOps: PartialFunction[Token, (Expr, Expr, Span) => Expr] =
    case Token.And(_) => Expr.And.apply
    case Token.Or(_)  => Expr.Or.apply

  private def binaryOpParser(operandParser: Parser[Token, Expr], operators: PartialFunction[Token, (Expr, Expr, Span) => Expr]): Parser[Token, Expr] =
    Parser.separatedByReduce(
      operandParser,
      matchingParser:
        case operators(operator) =>
          (left, right) => operator(left, right, left.span.merge(right.span))
    )

  val prefixOpParser: Parser[Token, Expr] = Parser.firstOf(
    matchingParser:
      case token @ prefixOps(operator) =>
        val term = prefixOpParser
        operator(term, token.span.merge(term.span))
    ,
    termParser
  )

  val binaryMulOpParser: Parser[Token, Expr] = binaryOpParser(prefixOpParser, binaryMulOps)
  val binaryAddOpParser: Parser[Token, Expr] = binaryOpParser(binaryMulOpParser, binaryAddOps)
  val binaryCompOpParser: Parser[Token, Expr] = binaryOpParser(binaryAddOpParser, binaryCompOps)
  val binaryBoolOpParser: Parser[Token, Expr] = binaryOpParser(binaryCompOpParser, binaryBoolOps)

  private val blockParser: Parser[Token, Expr] =
    Expr.Block.apply.tupled(tokenSpan(Parser.separatedBy(exprParser, tokenTypeParser[Token.Newline])))

  val optionalNewline: Parser[Token, Unit] = tryParserUnit(tokenTypeParser[Token.Newline])

  val ifParser: Parser[Token, Expr] = Expr.If.apply.tupled(
    tokenSpan(
      Parser.inOrder(
        tokenTypeParser[Token.If],
        exprParser,
        optionalNewline,
        tokenTypeParser[Token.Then],
        exprParser,
        Parser.firstOf(
          Parser.inOrder(
            optionalNewline,
            tokenTypeParser[Token.Else],
            exprParser
          ),
          Expr.Block(Nil, Span(0, 0))
        )
      )
    )
  )

  val forParser: Parser[Token, Expr] = Expr.For.apply.tupled(
    tokenSpan(
      Parser.inOrder(
        tokenTypeParser[Token.For],
        matchingParser:
          case Token.Ident(identifier, _) => identifier,
        optionalNewline,
        tokenTypeParser[Token.In],
        exprParser,
        optionalNewline,
        tokenTypeParser[Token.Do],
        exprParser
      )
    )
  )

  val whileParser: Parser[Token, Expr] = Expr.While.apply.tupled(
    tokenSpan(
      Parser.inOrder(
        tokenTypeParser[Token.While],
        exprParser,
        optionalNewline,
        tokenTypeParser[Token.Do],
        exprParser
      )
    )
  )

  val singleExpressionParser: Parser[Token, Expr] = Parser.firstOf(
    binaryBoolOpParser,
    ifParser,
    forParser,
    whileParser
  )

  val exprParser: Parser[Token, Expr] = Parser.expect(
    Parser.firstOf(
      Parser.inOrder(
        tokenTypeParser[Token.Indent],
        Parser.debug(blockParser, "block"),
        tokenTypeParser[Token.DeIndent]
      ),
      singleExpressionParser
    ),
    "Valid expression"
  )

  def apply(code: String): ParseResult[Char | Token, Expr] =
    val lexResult = Parser(code)(TokenLexer(code))
    lexResult.output.fold(lexResult.copy(output = None, errors = lexResult.errors)): tokens =>
      println(s"Tokens: ${tokens.zipWithIndex.mkString("\n")}")
      val parseResult = Parser(tokens.toIndexedSeq)(Parser.inOrder(blockParser, Parser.eof))
      parseResult.copy(errors = lexResult.errors ++ parseResult.errors)
