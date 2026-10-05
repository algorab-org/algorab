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
import org.algorab.util.FileName
import org.algorab.util.SourcePosition
import purelogic.*
import scala.annotation.tailrec

/**
 * A [[org.algorab.ast.raw\.Expr]] parser.
 */
object ExprParser:

  val literalParser: AlgorabParser[Token, Expr] = Parser.next match
    case Token.LBool(value, position)      => Expr.LBool(value, position)
    case Token.LInt(value, position)       => Expr.LInt(value, position)
    case Token.LFloat(value, position)     => Expr.LFloat(value, position)
    case Token.LChar(value, position)      => Expr.LChar(value, position)
    case Token.LString(value, position)    => Expr.LString(value, position)
    case Token.Ident(identifier, position) => Expr.VarCall(identifier, position)
    case _                                 => Parser.backtrack

  val termParser: AlgorabParser[Token, Expr] = Parser.firstOf(
    literalParser,
    Parser.inOrder(AlgorabParser.token[Token.ParenOpen], exprParser, Parser.commit(AlgorabParser.token[Token.ParenClosed]))
  )

  val applyParser: AlgorabParser[Token, Expr] =
    val (first, applications) = Parser.inOrder(
      termParser,
      AlgorabParser.repeat(
        AlgorabParser.tokenPosition(
          Parser.firstOf[Token, (Expr, SourcePosition) => Expr](
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
      case (expr, (op, position)) => op(expr, position.union(expr.position))

  private val prefixOps: PartialFunction[Token, (Expr, SourcePosition) => Expr] =
    case Token.Not(_)   => Expr.Not.apply
    case Token.Minus(_) => Expr.Minus.apply
    case Token.Plus(_)  => Expr.Plus.apply

  private val binaryMulOps: PartialFunction[Token, (Expr, Expr, SourcePosition) => Expr] =
    case Token.Mul(_)     => Expr.Mul.apply
    case Token.Div(_)     => Expr.Div.apply
    case Token.IntDiv(_)  => Expr.IntDiv.apply
    case Token.Percent(_) => Expr.Mod.apply

  private val binaryAddOps: PartialFunction[Token, (Expr, Expr, SourcePosition) => Expr] =
    case Token.Plus(_)  => Expr.Add.apply
    case Token.Minus(_) => Expr.Sub.apply

  private val binaryCompOps: PartialFunction[Token, (Expr, Expr, SourcePosition) => Expr] =
    case Token.EqualEqual(_)   => Expr.Equal.apply
    case Token.NotEqual(_)     => Expr.NotEqual.apply
    case Token.Greater(_)      => Expr.Greater.apply
    case Token.GreaterEqual(_) => Expr.GreaterEqual.apply
    case Token.Less(_)         => Expr.Less.apply
    case Token.LessEqual(_)    => Expr.LessEqual.apply

  private val binaryBoolOps: PartialFunction[Token, (Expr, Expr, SourcePosition) => Expr] =
    case Token.And(_) => Expr.And.apply
    case Token.Or(_)  => Expr.Or.apply

  private def binaryOpParser(
      operandParser: AlgorabParser[Token, Expr],
      operators: PartialFunction[Token, (Expr, Expr, SourcePosition) => Expr]
  ): AlgorabParser[Token, Expr] =
    Parser.separatedByReduce(
      operandParser,
      AlgorabParser.matching:
        case operators(operator) =>
          (left, right) => operator(left, right, left.position.union(right.position))
    )

  val prefixOpParser: AlgorabParser[Token, Expr] = Parser.firstOf(
    AlgorabParser.matching:
      case token @ prefixOps(operator) =>
        val term = prefixOpParser
        operator(term, token.position.union(term.position))
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
          Expr.Block(Nil, SourcePosition.at(read[FileInfo].name, 0, 0))
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
    val (mutable, name, tpe, expr, position) = AlgorabParser.tokenPosition(
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

    Definition.Val(name, tpe, expr, mutable, position)

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

  def importPathParser(acc: List[(Identifier, SourcePosition)]): AlgorabParser[Token, (List[(Identifier, SourcePosition)], Import.Selector)] =
    Parser.inOrder(
      AlgorabParser.token[Token.Dot],
      Parser.commit(
        Parser.expect(
          Parser.firstOf(
            (
              acc,
              selectorParser
            ),
            importPathParser(acc :+ AlgorabParser.tokenPosition(identifierParser)),
            (acc, Import.Selector.Simple.apply.tupled(AlgorabParser.tokenPosition(identifierParser)))
          ),
          "identifier or *"
        )
      )
    )

  val importParser: AlgorabParser[Token, Statement] = Import.apply.tupled(
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

  val statementParser: AlgorabParser[Token, Statement] = Parser.recoverWith(
    Parser.expect(
      Parser.firstOf(
        importParser,
        definitionParser,
        exprParser
      ),
      "Valid statement"
    ),
    RecoverStrategy.skipUntil(
      Parser.firstOf(
        AlgorabParser.token[Token.Newline],
        AlgorabParser.token[Token.DeIndent],
        Parser.eof
      ),
      Expr.Invalid(SourcePosition.at(read[FileInfo].name, 0, 0))
    )
  )

  val packageParser: AlgorabParser[Token, List[(Identifier, SourcePosition)]] = Parser.inOrder(
    AlgorabParser.token[Token.Package],
    Parser.separatedBy(
      AlgorabParser.tokenPosition(identifierParser),
      AlgorabParser.token[Token.Dot]
    )
  )

  val programParser: AlgorabParser[Token, Program] = Program.apply.tupled(
    Parser.inOrder(
      Parser.firstOf(packageParser, Nil),
      Parser.repeatDiscard(AlgorabParser.token[Token.Newline]),
      Parser.separatedBy(statementParser, AlgorabParser.token[Token.Newline])
    )
  )

  /**
   * Parse an expression from a list of tokens.
   *
   * @param tokens the tokens to read
   * @return the parsed [[org.algorab.ast.raw\.Program]]
   */
  def apply(tokens: List[Token]): Reader[FileInfo] ?=> AlgorabProgram[Program] =
    val result = Parser(tokens.toIndexedSeq)(Parser.inOrder(
      programParser,
      Parser.recoverWith(
        Parser.eof,
        RecoverStrategy.skipUntil(Parser.eof, ())
      )
    ))
    Writer.writeAll(result.errors.map(error =>
      ParsingError(error.expected, tokens(math.min(tokens.size, error.at)).position)
    ))
    Abort.extractOption(result.output, ())
