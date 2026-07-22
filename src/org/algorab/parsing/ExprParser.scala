package org.algorab.parsing

import io.github.iltotore.pureparser.*
import org.algorab.ast.Expr
import org.algorab.ast.Identifier
import org.algorab.ast.Type

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
    Parser.inOrder(tokenTypeParser[Token.ParenOpen], exprParser, Parser.commit(tokenTypeParser[Token.ParenClosed]))
  )

  val applyParser: Parser[Token, Expr] =
    val (first, applications) = Parser.inOrder(
      termParser,
      repeatParser(
        Parser.span(
          Parser.inOrder(
            tokenTypeParser[Token.ParenOpen],
            Parser.separatedBy(exprParser, tokenTypeParser[Token.Comma]),
            Parser.commit(tokenTypeParser[Token.ParenClosed])
          )
        )
      )
    )

    applications.foldLeft(first):
      case (expr, (params, span)) => Expr.Apply(expr, params, span.merge(expr.span))

  private val prefixOps: PartialFunction[Token, (Expr, Span) => Expr] =
    case Token.Not(_)   => Expr.Not.apply
    case Token.Minus(_) => Expr.Minus.apply
    case Token.Plus(_)  => Expr.Plus.apply

  private val binaryMulOps: PartialFunction[Token, (Expr, Expr, Span) => Expr] =
    case Token.Mul(_)     => Expr.Mul.apply
    case Token.Div(_)     => Expr.Div.apply
    case Token.IntDiv(_)  => Expr.IntDiv.apply
    case Token.Percent(_) => Expr.Mod.apply

  private val binaryAddOps: PartialFunction[Token, (Expr, Expr, Span) => Expr] =
    case Token.Plus(_)  => Expr.Add.apply
    case Token.Minus(_) => Expr.Sub.apply

  private val binaryCompOps: PartialFunction[Token, (Expr, Expr, Span) => Expr] =
    case Token.EqualEqual(_)   => Expr.Equal.apply
    case Token.NotEqual(_)     => Expr.NotEqual.apply
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
    applyParser
  )

  val binaryMulOpParser: Parser[Token, Expr] = binaryOpParser(prefixOpParser, binaryMulOps)
  val binaryAddOpParser: Parser[Token, Expr] = binaryOpParser(binaryMulOpParser, binaryAddOps)
  val binaryCompOpParser: Parser[Token, Expr] = binaryOpParser(binaryAddOpParser, binaryCompOps)
  val binaryBoolOpParser: Parser[Token, Expr] = binaryOpParser(binaryCompOpParser, binaryBoolOps)

  private val blockParser: Parser[Token, Expr] =
    Expr.Block.apply.tupled(tokenSpan(Parser.separatedBy(exprParser, tokenTypeParser[Token.Newline])))

  private val identifierParser: Parser[Token, Identifier] = matchingParser:
    case Token.Ident(identifier, _) => identifier

  val typeParser: Parser[Token, Type] = Type.Ref(identifierParser)

  val ifParser: Parser[Token, Expr] = Expr.If.apply.tupled(
    tokenSpan(
      Parser.inOrder(
        tokenTypeParser[Token.If],
        Parser.commit(Parser.inOrder(
          exprParser,
          tokenTypeParser[Token.Then],
          exprParser
        )),
        Parser.firstOf(
          Parser.inOrder(
            tokenTypeParser[Token.Else],
            Parser.commit(exprParser)
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
        Parser.commit(Parser.inOrder(
          identifierParser,
          tokenTypeParser[Token.In],
          exprParser,
          tokenTypeParser[Token.Do],
          exprParser
        ))
      )
    )
  )

  val whileParser: Parser[Token, Expr] = Expr.While.apply.tupled(
    tokenSpan(
      Parser.inOrder(
        tokenTypeParser[Token.While],
        Parser.commit(Parser.inOrder(
          exprParser,
          tokenTypeParser[Token.Do],
          exprParser
        ))
      )
    )
  )

  val valDefParser: Parser[Token, Expr] =
    val (mutable, name, tpe, expr, span) = tokenSpan(
      Parser.inOrder(
        Parser.firstOf(Parser.as(tokenTypeParser[Token.Mut], true), false),
        tokenTypeParser[Token.Val],
        Parser.commit(Parser.inOrder(
          identifierParser,
          Parser.firstOf(
            Parser.inOrder(tokenTypeParser[Token.Colon], Parser.commit(typeParser)),
            Type.Inferred
          ),
          tokenTypeParser[Token.Equal],
          exprParser
        ))
      )
    )

    Expr.ValDef(name, tpe, expr, mutable, span)

  val assignParser: Parser[Token, Expr] = Expr.Assign.apply.tupled(
    tokenSpan(
      Parser.inOrder(
        identifierParser,
        tokenTypeParser[Token.Equal],
        Parser.commit(exprParser)
      )
    )
  )

  val funDefParser: Parser[Token, Expr] = Expr.FunDef.apply.tupled(
    tokenSpan(
      Parser.inOrder(
        tokenTypeParser[Token.Def],
        Parser.commit(Parser.inOrder(
          identifierParser,
          tokenTypeParser[Token.ParenOpen],
          Parser.separatedBy(
            Parser.inOrder(identifierParser, tokenTypeParser[Token.Colon], typeParser),
            tokenTypeParser[Token.Comma]
          ),
          tokenTypeParser[Token.ParenClosed],
          Parser.firstOf(
            Parser.inOrder(tokenTypeParser[Token.Colon], Parser.commit(typeParser)),
            Type.Inferred
          ),
          tokenTypeParser[Token.Equal],
          exprParser
        ))
      )
    )
  )

  val singleExpressionParser: Parser[Token, Expr] = Parser.firstOf(
    ifParser,
    forParser,
    whileParser,
    valDefParser,
    assignParser,
    funDefParser,
    binaryBoolOpParser
  )

  val exprParser: Parser[Token, Expr] = Parser.expect(
    Parser.firstOf(
      Parser.inOrder(
        tokenTypeParser[Token.Indent],
        blockParser,
        tokenTypeParser[Token.DeIndent]
      ),
      singleExpressionParser
    ),
    "Valid expression"
  )

  def apply(code: String): ParseResult[Char | Token, Expr] =
    val lexResult = Parser(code)(TokenLexer(code))
    lexResult.output.fold(lexResult.copy(output = None, errors = lexResult.errors)): tokens =>
      val parseResult = Parser(tokens.toIndexedSeq)(Parser.inOrder(blockParser, Parser.eof))
      parseResult.copy(errors = lexResult.errors ++ parseResult.errors)
