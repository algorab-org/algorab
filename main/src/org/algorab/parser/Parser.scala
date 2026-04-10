package org.algorab.parser

import kyo.*
import org.algorab.ast.Identifier
import org.algorab.ast.untpd.Expr
import org.algorab.ast.untpd.Type
import org.algorab.parser.debug
import scala.annotation.nowarn

object Parser:

  given CanEqual[Unit, Unit | Type] = CanEqual.derived

  private def separatedByReduce[A, In](element: A < Parse[In], sep: ((A, A) => A) < Parse[In])(using Tag[In], Frame): A < Parse[In] =
    Parse
      .inOrder(
        element,
        Parse.repeat(Parse.inOrder(sep, Parse.require(element)))
      )
      .map((firstTerm, others) =>
        others.foldLeft(firstTerm):
          case (left, (reduce, right)) => reduce(left, right)
      )

  private def binaryOperator[A, In](element: A < Parse[In], operators: Map[In, (A, A) => A])(using Tag[In], Frame): A < Parse[In] =
    separatedByReduce(
      element,
      Parse.anyMatch(operators.get.unlift)
    )

  val parseIdentifier: Identifier < Parse[Token] = Parse.anyMatch:
    case Token.Ident(identifier) => identifier

  val parseLiteral: Expr < Parse[Token] = Parse.anyMatch:
    case Token.LBool(value)   => Expr.LBool(value)
    case Token.LInt(value)    => Expr.LInt(value)
    case Token.LFloat(value)  => Expr.LFloat(value)
    case Token.LString(value) => Expr.LString(value)
    case Token.Ident(name)    => Expr.VarCall(name)

  lazy val parseTerm: Expr < Parse[Token] = withErrorMessage(
    Parse.firstOf(
      parseLiteral,
      Parse.between(Parse.literal(Token.ParenOpen), Parse.require(parseExpr), Parse.literal(Token.ParenClosed))
    ),
    "Invalid term"
  )

  lazy val parseApply: Expr < Parse[Token] = Parse.inOrder(
    parseTerm,
    Parse.repeat(
      Parse.firstOf(
        // variable(...,...)
        Parse.inOrder(
          Parse.literal(Token.ParenOpen),
          Parse.separatedBy(parseExpr, Parse.literal(Token.Comma)),
          Parse.literal(Token.ParenClosed)
        ).map((_, args, _) => Expr.Apply(_, args)),
        // variable[...,...]
        Parse.inOrder(
          Parse.literal(Token.SquareOpen),
          Parse.separatedBy(parseType, Parse.literal(Token.Comma)),
          Parse.literal(Token.SquareClosed)
        ).map((_, types, _) => Expr.TypeApply(_, types)),
        // variable.member
        Parse.inOrder(
          Parse.literal(Token.Dot),
          Parse.require(parseIdentifier)
        ).map((_, member) => Expr.Select(_, member))
      )
    )
  ).map((expr, apps) => apps.foldLeft(expr)((expr, app) => app(expr)))

  // Operators

  val unaryOps: Map[Token, Expr => Expr] = Map(
    Token.Plus -> Expr.Plus.apply,
    Token.Minus -> Expr.Minus.apply,
    Token.Not -> Expr.Not.apply
  )

  val addOps: Map[Token, (Expr, Expr) => Expr] = Map(
    Token.Plus -> Expr.Add.apply,
    Token.Minus -> Expr.Sub.apply
  )

  val mulOps: Map[Token, (Expr, Expr) => Expr] = Map(
    Token.Mul -> Expr.Mul.apply,
    Token.Div -> Expr.Div.apply,
    Token.IntDiv -> Expr.IntDiv.apply,
    Token.Percent -> Expr.Mod.apply
  )

  val compOps: Map[Token, (Expr, Expr) => Expr] = Map(
    Token.Less -> Expr.Less.apply,
    Token.LessEqual -> Expr.LessEqual.apply,
    Token.Greater -> Expr.Greater.apply,
    Token.GreaterEqual -> Expr.GreaterEqual.apply,
    Token.EqualEqual -> Expr.Equal.apply,
    Token.NotEqual -> Expr.NotEqual.apply
  )

  val boolOps: Map[Token, (Expr, Expr) => Expr] = Map(
    Token.And -> Expr.And.apply,
    Token.Or -> Expr.Or.apply
  )

  lazy val parseUnary: Expr < Parse[Token] =
    Parse.firstOf(
      Parse.inOrder(
        Parse.anyMatch(unaryOps.get.unlift),
        Parse.require(parseApply)
      ).map((op, expr) => op(expr)),
      parseApply
    )

  // Each binary ops parser gives precedence to the operator in its left argument
  // Each binary ops parser also includes operators with more precedence than itself (e.g parseAdd includes parseMul)

  lazy val parseMul: Expr < Parse[Token] = binaryOperator(parseUnary, mulOps)

  lazy val parseAdd: Expr < Parse[Token] = binaryOperator(parseMul, addOps)

  lazy val parseComp: Expr < Parse[Token] = binaryOperator(parseAdd, compOps)

  lazy val parseBool: Expr < Parse[Token] = binaryOperator(parseComp, boolOps)

  lazy val parseAssign: Expr < Parse[Token] =
    Parse.inOrder(
      parseIdentifier,
      Parse.literal(Token.Equal),
      Parse.require(parseBlockOrExpr)
    ).map((name, _, expr) => Expr.Assign(name, expr))

  lazy val parseTypeTerm: Type < Parse[Token] = Parse.firstOf(
    parseIdentifier.map(Type.Ref.apply),
    Parse.between(
      Parse.literal(Token.ParenOpen),
      Parse.separatedBy(parseType, Parse.literal(Token.Comma)),
      Parse.literal(Token.ParenClosed)
    ).map(types =>
      types.length match
        case 0 => Parse.fail("Expected type")
        case 1 => types.head
        case _ => Type.Tuple(types)
    )
  )

  lazy val parseTypeApply: Type < Parse[Token] = Parse.firstOf(
    Parse.inOrder(
      parseTypeTerm,
      Parse.literal(Token.SquareOpen),
      Parse.separatedBy(parseType, Parse.literal(Token.Comma)),
      Parse.literal(Token.SquareClosed)
    ).map((base, _, args, _) => Type.Apply(base, args)),
    parseTypeTerm
  )

  @nowarn("msg=not.*?exhaustive") // Bug in exhaustivity check
  lazy val parseType: Type < Parse[Token] = Parse.separatedBy(
    Parse.firstOf(
      Parse.inOrder(Parse.literal(Token.ParenOpen), Parse.literal(Token.ParenClosed)).unit,
      parseTypeApply
    ),
    Parse.literal(Token.DoubleArrow)
  ).map:
    case Chunk()     => Parse.fail("Type expected")
    case types :+ () => Parse.fail("A function cannot return `()`. Use `Unit` instead")
    case types :+ (last: Type) => types.foldRight(last)((left, right) =>
        left match
          case ()                 => Type.Fun(Chunk.empty, right)
          case Type.Tuple(params) => Type.Fun(params, right)
          case other: Type        => Type.Fun(Chunk(other), right)
      )

  lazy val parseValDef: Expr < Parse[Token] =
    Parse.inOrder(
      Parse.attempt(Parse.literal(Token.Mut)).map(_.isDefined),
      Parse.literal(Token.Val),
      Parse.require(parseIdentifier),
      Parse.firstOf(
        Parse.literal(Token.Colon).andThen(Parse.require(parseType)),
        Type.Inferred
      ),
      Parse.require(Parse.literal(Token.Equal)),
      Parse.require(parseBlockOrExpr)
    ).map((mutable, _, name, tpe, _, expr) => Expr.ValDef(name, tpe, expr, mutable))

  lazy val parseFunDef: Expr < Parse[Token] =
    Parse.inOrder(
      Parse.literal(Token.Def),
      Parse.require(parseIdentifier),
      // Generic type parameters
      Parse.attempt(
        Parse.between(
          Parse.literal(Token.SquareOpen),
          Parse.separatedBy(
            Parse.require(parseIdentifier),
            Parse.literal(Token.Comma)
          ),
          Parse.literal(Token.SquareClosed)
        )
      ),
      // Arguments
      Parse.require(
        Parse.between(
          Parse.literal(Token.ParenOpen),
          Parse.separatedBy(
            Parse.inOrder(
              parseIdentifier,
              Parse.literal(Token.Colon),
              Parse.require(parseType)
            ),
            Parse.literal(Token.Comma)
          ),
          Parse.literal(Token.ParenClosed)
        )
      ),
      // Return type
      Parse.require(Parse.literal(Token.Colon)),
      Parse.require(parseType),
      // Body
      Parse.require(Parse.literal(Token.Equal)),
      Parse.require(parseBlockOrExpr)
    ).map((_, name, typeParamsOpt, params, _, retType, _, body) =>
      val typeParams = typeParamsOpt.getOrElse(Chunk())
      Expr.FunDef(name, typeParams, params.map((id, _, tpe) => (id, tpe)), retType, body)
    )

  lazy val parseIf: Expr < Parse[Token] =
    Parse.inOrder(
      Parse.literal(Token.If),
      Parse.require(parseBlockOrExpr),
      Parse.require(Parse.literal(Token.Then)),
      Parse.require(parseBlockOrExpr),
      Parse.firstOf(
        Parse.literal(Token.Else).andThen(Parse.require(parseBlockOrExpr)),
        Expr.VarCall(Identifier("Unit"))
      )
    ).map((_, cond, _, thenBranch, elseBranch) => Expr.If(cond, thenBranch, elseBranch))

  lazy val parseFor: Expr < Parse[Token] =
    Parse.inOrder(
      Parse.literal(Token.For),
      Parse.require(parseIdentifier),
      Parse.require(Parse.literal(Token.In)),
      Parse.require(parseBlockOrExpr),
      Parse.require(Parse.literal(Token.Do)),
      Parse.require(parseBlockOrExpr)
    ).map((_, id, _, range, _, body) => Expr.For(id, range, body))

  lazy val parseWhile: Expr < Parse[Token] =
    Parse.inOrder(
      Parse.literal(Token.While),
      Parse.require(parseBlockOrExpr),
      Parse.require(Parse.literal(Token.Do)),
      Parse.require(parseBlockOrExpr)
    ).map((_, cond, _, body) => Expr.While(cond, body))

  lazy val parseClass: Expr < Parse[Token] =
    Parse.inOrder(
      Parse.literal(Token.Class),
      Parse.require(parseIdentifier),
      // Optional generic type parameters: class Foo[A, B]:
      Parse.attempt(
        Parse.between(
          Parse.literal(Token.SquareOpen),
          Parse.separatedBy(Parse.require(parseIdentifier), Parse.literal(Token.Comma)),
          Parse.literal(Token.SquareClosed)
        )
      ),
      // Optional constructor parameters: class Foo(x: Int, y: String):
      Parse.attempt(
        Parse.between(
          Parse.literal(Token.ParenOpen),
          Parse.separatedBy(
            Parse.inOrder(
              parseIdentifier,
              Parse.literal(Token.Colon),
              Parse.require(parseType)
            ).map((id, _, tpe) => (id, tpe)),
            Parse.literal(Token.Comma)
          ),
          Parse.literal(Token.ParenClosed)
        )
      ),

      // Class can be empty
      Parse.attempt(
        Parse.literal(Token.Equal).andThen(parseBlockOrExpr)
      )
    ).map((_, name, typeParamsOpt, parametersOpt, maybeBody) =>
      // Generic types
      val typeParams = typeParamsOpt.getOrElse(Chunk.empty)
      // Body may or may not be present
      val body = maybeBody.fold(Chunk.empty)(_.expressions)
      Expr.ClassDef(name, typeParams, parametersOpt.getOrElse(Chunk.empty), body)
    )

  lazy val parseLambda: Expr < Parse[Token] =
    Parse.inOrder(
      Parse.literal(Token.ParenOpen),
      Parse.separatedBy(
        Parse.inOrder(
          parseIdentifier,
          Parse.literal(Token.Colon),
          Parse.require(parseType)
        ).map((id, _, tpe) => (id, tpe)),
        Parse.literal(Token.Comma)
      ),
      Parse.literal(Token.ParenClosed),
      Parse.require(Parse.literal(Token.DoubleArrow)),
      Parse.require(parseBlockBody)
    ).map((_, params, _, _, body) =>
      // We don't need an Expr.Lambda where we're going ! :D
      Expr.Block(Chunk(
        Expr.FunDef(Identifier("lambda"), Chunk.empty, params, Type.Inferred, body),
        Expr.VarCall(Identifier("lambda"))
      ))
    )

  lazy val parseBlockBody: Expr.Block < Parse[Token] =
    Parse.separatedBy(
      parseExpr,
      Parse.literal(Token.Newline),
      allowTrailing = true
    ).map(Expr.Block.apply)

  lazy val parseBlockOrExpr: Expr.Block < Parse[Token] =
    Parse.between(
      Parse.literal(Token.Indent),
      parseBlockBody,
      Parse.literal(Token.DeIndent)
    )

  lazy val parseExpr: Expr < Parse[Token] = Parse.firstOf(Seq(
    () => parseAssign,
    () => parseValDef,
    () => parseFunDef,
    () => parseIf,
    () => parseFor,
    () => parseWhile,
    () => parseClass,
    () => parseLambda,
    () => parseBool
  ))

  val parseAst: Expr < Parse[Token] = Parse.entireInput(
    Parse.between(
      Parse.readWhile[Token](_ == Token.Newline),
      Parse.require(parseBlockBody),
      Parse.readWhile[Token](_ == Token.Newline)
    )
  )
