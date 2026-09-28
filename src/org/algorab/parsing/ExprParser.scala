package org.algorab.parsing

import io.github.iltotore.pureparser.*
import org.algorab.AlgorabProgram
import org.algorab.ast.Identifier
import org.algorab.ast.raw.Definition
import org.algorab.ast.raw.Expr
import org.algorab.ast.raw.Import
import org.algorab.ast.raw.Program
import org.algorab.ast.raw.Statement
import org.algorab.ast.raw.Type
import purelogic.Abort
import purelogic.Writer
import scala.annotation.tailrec
import purelogic.Reader
import org.algorab.util.FileName

/**
 * A [[org.algorab.ast.raw\.Expr]] parser.
 */
object ExprParser:

  val literalParser: AlgorabParser[Token, Expr] = Parser.next match
    case Token.LBool(value, span)      => Expr.LBool(value, span)
    case Token.LInt(value, span)       => Expr.LInt(value, span)
    case Token.LFloat(value, span)     => Expr.LFloat(value, span)
    case Token.LChar(value, span)      => Expr.LChar(value, span)
    case Token.LString(value, span)    => Expr.LString(value, span)
    case Token.Ident(identifier, span) => Expr.VarCall(identifier, span)
    case _                             => Parser.backtrack

  val termParser: AlgorabParser[Token, Expr] = Parser.firstOf(
    literalParser,
    Parser.inOrder(AlgorabParser.token[Token.ParenOpen], exprParser, Parser.commit(AlgorabParser.token[Token.ParenClosed]))
  )

  val applyParser: AlgorabParser[Token, Expr] =
    val (first, applications) = Parser.inOrder(
      termParser,
      AlgorabParser.repeat(
        AlgorabParser.tokenPosition(
          Parser.firstOf[Token, (Expr, Span) => Expr](
            AlgorabParser.map(
              Parser.inOrder(
                AlgorabParser.token[Token.ParenOpen],
                Parser.separatedBy(exprParser, AlgorabParser.token[Token.Comma]),
                Parser.commit(AlgorabParser.token[Token.ParenClosed])
              )
            )(params => Expr.Apply(_, params, _)),
            AlgorabParser.map(
              Parser.inOrder(
                AlgorabParser.token[Token.Dot],
                identifierParser
              )
            )(member => Expr.Select(_, member, _))
          )
        )
      )
    )

    applications.foldLeft(first):
      case (expr, (op, span)) => op(expr, span.merge(expr.span))

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

  private def binaryOpParser(operandParser: AlgorabParser[Token, Expr], operators: PartialFunction[Token, (Expr, Expr, Span) => Expr]): AlgorabParser[Token, Expr] =
    Parser.separatedByReduce(
      operandParser,
      AlgorabParser.matching:
        case operators(operator) =>
          (left, right) => operator(left, right, left.span.merge(right.span))
    )

  val prefixOpParser: AlgorabParser[Token, Expr] = Parser.firstOf(
    AlgorabParser.matching:
      case token @ prefixOps(operator) =>
        val term = prefixOpParser
        operator(term, token.span.merge(term.span))
    ,
    applyParser
  )

  val binaryMulOpParser: AlgorabParser[Token, Expr] = binaryOpParser(prefixOpParser, binaryMulOps)
  val binaryAddOpParser: AlgorabParser[Token, Expr] = binaryOpParser(binaryMulOpParser, binaryAddOps)
  val binaryCompOpParser: AlgorabParser[Token, Expr] = binaryOpParser(binaryAddOpParser, binaryCompOps)
  val binaryBoolOpParser: AlgorabParser[Token, Expr] = binaryOpParser(binaryCompOpParser, binaryBoolOps)

  private val blockParser: AlgorabParser[Token, Expr] =
    Expr.Block.apply.tupled(AlgorabParser.tokenPosition(Parser.separatedBy(statementParser, AlgorabParser.token[Token.Newline])))

  private val identifierParser: AlgorabParser[Token, Identifier] = AlgorabParser.matching:
    case Token.Ident(identifier, _) => identifier

  val typeParser: AlgorabParser[Token, Type] = Type.Ref(identifierParser)

  val ifParser: AlgorabParser[Token, Expr] = Expr.If.apply.tupled(
    AlgorabParser.tokenPosition(
      Parser.inOrder(
        AlgorabParser.token[Token.If],
        Parser.commit(Parser.inOrder(
          exprParser,
          AlgorabParser.token[Token.Then],
          exprParser
        )),
        Parser.firstOf(
          Parser.inOrder(
            AlgorabParser.token[Token.Else],
            Parser.commit(exprParser)
          ),
          Expr.Block(Nil, Span(0, 0))
        )
      )
    )
  )

  val forParser: AlgorabParser[Token, Expr] = Expr.For.apply.tupled(
    AlgorabParser.tokenPosition(
      Parser.inOrder(
        AlgorabParser.token[Token.For],
        Parser.commit(Parser.inOrder(
          identifierParser,
          AlgorabParser.token[Token.In],
          exprParser,
          AlgorabParser.token[Token.Do],
          exprParser
        ))
      )
    )
  )

  val whileParser: AlgorabParser[Token, Expr] = Expr.While.apply.tupled(
    AlgorabParser.tokenPosition(
      Parser.inOrder(
        AlgorabParser.token[Token.While],
        Parser.commit(Parser.inOrder(
          exprParser,
          AlgorabParser.token[Token.Do],
          exprParser
        ))
      )
    )
  )

  val valDefParser: AlgorabParser[Token, Definition] =
    val (mutable, name, tpe, expr, span) = AlgorabParser.tokenPosition(
      Parser.inOrder(
        Parser.firstOf(Parser.as(AlgorabParser.token[Token.Mut], true), false),
        AlgorabParser.token[Token.Val],
        Parser.commit(Parser.inOrder(
          identifierParser,
          Parser.firstOf(
            Parser.inOrder(AlgorabParser.token[Token.Colon], Parser.commit(typeParser)),
            Type.Inferred
          ),
          AlgorabParser.token[Token.Equal],
          exprParser
        ))
      )
    )

    Definition.Val(name, tpe, expr, mutable, span)

  val assignParser: AlgorabParser[Token, Expr] = Expr.Assign.apply.tupled(
    AlgorabParser.tokenPosition(
      Parser.inOrder(
        identifierParser,
        AlgorabParser.token[Token.Equal],
        Parser.commit(exprParser)
      )
    )
  )

  val funDefParser: AlgorabParser[Token, Definition] = Definition.Function.apply.tupled(
    AlgorabParser.tokenPosition(
      Parser.inOrder(
        AlgorabParser.token[Token.Def],
        Parser.commit(Parser.inOrder(
          identifierParser,
          AlgorabParser.token[Token.ParenOpen],
          Parser.separatedBy(
            Parser.inOrder(identifierParser, AlgorabParser.token[Token.Colon], typeParser),
            AlgorabParser.token[Token.Comma]
          ),
          AlgorabParser.token[Token.ParenClosed],
          Parser.firstOf(
            Parser.inOrder(AlgorabParser.token[Token.Colon], Parser.commit(typeParser)),
            Type.Inferred
          ),
          AlgorabParser.token[Token.Equal],
          exprParser
        ))
      )
    )
  )

  val selectorParser: AlgorabParser[Token, Import.Selector] = Parser.firstOf(
    Import.Selector.Wildcard(AlgorabParser.tokenPosition(AlgorabParser.token[Token.Mul])),
    Import.Selector.Rename.apply.tupled(AlgorabParser.tokenPosition(
      Parser.inOrder(
        identifierParser,
        AlgorabParser.token[Token.As],
        identifierParser
      )
    ))
  )

  def importPathParser(acc: List[(Identifier, Span)]): AlgorabParser[Token, (List[(Identifier, Span)], Import.Selector)] =
    Parser.inOrder(
      AlgorabParser.token[Token.Dot],
      Parser.firstOf(
        (
          acc,
          selectorParser
        ),
        importPathParser(acc :+ AlgorabParser.tokenPosition(identifierParser)),
        (acc, Import.Selector.Simple.apply.tupled(AlgorabParser.tokenPosition(identifierParser)))
      )
    )

  val importParser: AlgorabParser[Token, Import] = Import.apply.tupled(
    AlgorabParser.tokenPosition(
      Parser.inOrder(
        AlgorabParser.token[Token.Import],
        importPathParser(List(AlgorabParser.tokenPosition(identifierParser)))
      )
    )
  )

  val definitionParser: AlgorabParser[Token, Definition] = Parser.firstOf(
    valDefParser,
    funDefParser
  )

  val exprParser: AlgorabParser[Token, Expr] = Parser.expect(
    Parser.firstOf(
      Parser.inOrder(
        AlgorabParser.token[Token.Indent],
        blockParser,
        AlgorabParser.token[Token.DeIndent]
      ),
      ifParser,
      forParser,
      whileParser,
      assignParser,
      binaryBoolOpParser
    ),
    "Valid expression"
  )

  val statementParser: AlgorabParser[Token, Statement] = Parser.expect(
    Parser.firstOf(
      importParser,
      definitionParser,
      exprParser
    ),
    "Valid statement"
  )

  val packageParser: AlgorabParser[Token, List[(Identifier, Span)]] = Parser.inOrder(
    AlgorabParser.token[Token.Package],
    Parser.separatedBy(
      AlgorabParser.tokenPosition(identifierParser),
      AlgorabParser.token[Token.Dot]
    )
  )

  val programParser: AlgorabParser[Token, Program] = Program.apply.tupled(
    Parser.inOrder(
      Parser.firstOf(packageParser, Nil),
      Parser.repeatDiscard0(AlgorabParser.token[Token.Newline]),
      Parser.separatedBy(statementParser, AlgorabParser.token[Token.Newline])
    )
  )

  /**
   * Parse an expression from a list of tokens.
   *
   * @param tokens the tokens to read
   * @return the parsed [[org.algorab.ast.raw\.Program]]
   */
  def apply(tokens: List[Token]): Reader[FileName] ?=> AlgorabProgram[Program] =
    val result = Parser(tokens.toIndexedSeq)(Parser.inOrder(programParser, Parser.eof))
    Writer.writeAll(result.errors)
    Abort.extractOption(result.output, ())
