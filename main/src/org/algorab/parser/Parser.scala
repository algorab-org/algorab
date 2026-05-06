/**
 * Algorab parser: transforms a flat [[Token]] stream into an untyped expression tree.
 *
 * [[Parser]] is a collection of `Parse[Token]` combinators that implement the complete
 * Algorab grammar.  The grammar is expression-oriented: every construct (including function
 * and class definitions) is an expression that produces a value.
 *
 * Operator precedence, from lowest to highest:
 *   1. Assignment / definitions (`=`)
 *   1. Boolean (`and`, `or`)
 *   1. Comparison (`==`, `!=`, `<`, `<=`, `>`, `>=`)
 *   1. Additive (`+`, `-`)
 *   1. Multiplicative (`*`, `/`, `//`, `%`)
 *   1. Unary (`+`, `-`, `not`)
 *   1. Application / selection (`f(…)`, `f[…]`, `.member`)
 *   1. Atoms (literals, identifiers, parenthesised expressions)
 *
 * The main entry point is [[parseAst]], which parses a complete Algorab program.
 */
package org.algorab.parser

import kyo.*
import org.algorab.ast.Identifier
import org.algorab.ast.untpd.Expr
import org.algorab.ast.untpd.Type
import org.algorab.parser.debug
import scala.annotation.nowarn

/**
 * Parser combinators for the Algorab grammar.
 *
 * All parsers are `lazy val`s to allow mutual recursion between expression and
 * type parsers without causing initialisation-order issues.
 */
object Parser:

  given CanEqual[Unit, Unit | Type] = CanEqual.derived

  /**
   * Parses a left-associative list of `element`s separated by a binary `sep` combinator.
   *
   * The separator parser returns a function `(A, A) => A` that folds the result.
   * Elements are combined left-to-right.
   *
   * @param element the parser for each operand
   * @param sep     a parser that consumes the operator and returns the combining function
   * @return the left-folded result of all elements
   */
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

  /**
   * Builds a left-associative binary-operator parser from a map of operator tokens.
   *
   * @param element   the parser for the operands
   * @param operators a map from operator token to the AST-combining function
   * @return a parser that handles one or more `element (op element)*` sequences
   */
  private def binaryOperator[A, In](element: A < Parse[In], operators: Map[In, (A, A) => A])(using Tag[In], Frame): A < Parse[In] =
    separatedByReduce(
      element,
      Parse.anyMatch(operators.get.unlift)
    )

  /**
   * Parses a non-blank identifier token.
   *
   * Matches any [[Token.Ident]] and unwraps its [[Identifier]] value.
   */
  val parseIdentifier: Identifier < Parse[Token] = Parse.anyMatch:
    case Token.Ident(identifier) => identifier

  /**
   * Parses a single literal or variable-reference expression.
   *
   * Matches: `LBool`, `LInt`, `LFloat`, `LString`, and bare identifier references
   * (which become [[Expr.VarCall]]).
   */
  val parseLiteral: Expr < Parse[Token] = Parse.anyMatch:
    case Token.LBool(value)   => Expr.LBool(value)
    case Token.LInt(value)    => Expr.LInt(value)
    case Token.LFloat(value)  => Expr.LFloat(value)
    case Token.LString(value) => Expr.LString(value)
    case Token.Ident(name)    => Expr.VarCall(name)

  /**
   * Parses an atomic expression: a literal or a parenthesised expression.
   *
   * This is the lowest-level expression parser; all higher-precedence parsers bottom out here.
   */
  lazy val parseTerm: Expr < Parse[Token] = withErrorMessage(
    Parse.firstOf(
      parseLiteral,
      Parse.between(Parse.literal(Token.ParenOpen), Parse.require(parseExpr), Parse.literal(Token.ParenClosed))
    ),
    "Invalid term"
  )

  /**
   * Parses a term followed by zero or more postfix operations.
   *
   * Postfix operations (applied left-to-right) include:
   *   - Function application: `expr(arg1, arg2, …)` → [[Expr.Apply]]
   *   - Type application:     `expr[T1, T2, …]`     → [[Expr.TypeApply]]
   *   - Member selection:     `expr.member`          → [[Expr.Select]]
   */
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

  // Operator tables

  /** Unary prefix operators and their AST constructors. */
  val unaryOps: Map[Token, Expr => Expr] = Map(
    Token.Plus -> Expr.Plus.apply,
    Token.Minus -> Expr.Minus.apply,
    Token.Not -> Expr.Not.apply
  )

  /** Additive binary operators and their AST constructors. */
  val addOps: Map[Token, (Expr, Expr) => Expr] = Map(
    Token.Plus -> Expr.Add.apply,
    Token.Minus -> Expr.Sub.apply
  )

  /** Multiplicative binary operators and their AST constructors. */
  val mulOps: Map[Token, (Expr, Expr) => Expr] = Map(
    Token.Mul -> Expr.Mul.apply,
    Token.Div -> Expr.Div.apply,
    Token.IntDiv -> Expr.IntDiv.apply,
    Token.Percent -> Expr.Mod.apply
  )

  /** Comparison binary operators and their AST constructors. */
  val compOps: Map[Token, (Expr, Expr) => Expr] = Map(
    Token.Less -> Expr.Less.apply,
    Token.LessEqual -> Expr.LessEqual.apply,
    Token.Greater -> Expr.Greater.apply,
    Token.GreaterEqual -> Expr.GreaterEqual.apply,
    Token.EqualEqual -> Expr.Equal.apply,
    Token.NotEqual -> Expr.NotEqual.apply
  )

  /** Boolean binary operators and their AST constructors. */
  val boolOps: Map[Token, (Expr, Expr) => Expr] = Map(
    Token.And -> Expr.And.apply,
    Token.Or -> Expr.Or.apply
  )

  // Expression parsers by precedence level

  /** Parses a unary prefix expression (`+`, `-`, `not`) or falls through to [[parseApply]]. */
  lazy val parseUnary: Expr < Parse[Token] =
    Parse.firstOf(
      Parse.inOrder(
        Parse.anyMatch(unaryOps.get.unlift),
        Parse.require(parseApply)
      ).map((op, expr) => op(expr)),
      parseApply
    )

  /** Parses multiplicative expressions (`*`, `/`, `//`, `%`). */
  lazy val parseMul: Expr < Parse[Token] = binaryOperator(parseUnary, mulOps)

  /** Parses additive expressions (`+`, `-`). */
  lazy val parseAdd: Expr < Parse[Token] = binaryOperator(parseMul, addOps)

  /** Parses comparison expressions (`<`, `<=`, `>`, `>=`, `==`, `!=`). */
  lazy val parseComp: Expr < Parse[Token] = binaryOperator(parseAdd, compOps)

  /** Parses boolean expressions (`and`, `or`). */
  lazy val parseBool: Expr < Parse[Token] = binaryOperator(parseComp, boolOps)

  /**
   * Parses a simple assignment expression (`name = expr`).
   *
   * Note: this does ''not'' consume `val` or `mut`; it is intended for reassignment
   * to existing mutable variables.
   */
  lazy val parseAssign: Expr < Parse[Token] =
    Parse.inOrder(
      parseIdentifier,
      Parse.literal(Token.Equal),
      Parse.require(parseBlockOrExpr)
    ).map((name, _, expr) => Expr.Assign(name, expr))

  // Type parsers

  /** Parses an atomic type expression: a named type or a parenthesised type/tuple. */
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

  /** Parses a generic type application (e.g. `Array[Int]`) or falls through to [[parseTypeTerm]]. */
  lazy val parseTypeApply: Type < Parse[Token] = Parse.firstOf(
    Parse.inOrder(
      parseTypeTerm,
      Parse.literal(Token.SquareOpen),
      Parse.separatedBy(parseType, Parse.literal(Token.Comma)),
      Parse.literal(Token.SquareClosed)
    ).map((base, _, args, _) => Type.Apply(base, args)),
    parseTypeTerm
  )

  /**
   * Parses a complete type expression, including function types.
   *
   * Function types use `=>` as the separator and are right-associative.
   * The unit input type `()` is recognised as an empty parameter list.
   * A trailing `()` in the output position is rejected with an error message
   * (users must write `Unit` explicitly).
   */
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

  // Declaration parsers

  /**
   * Parses a `val` or `mut val` variable-binding expression.
   *
   * Syntax: `[mut] val name [: Type] = expr`
   */
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

  /**
   * Parses a `def` function definition.
   *
   * Syntax: `def name [TypeParams] (params): RetType = body`
   *
   * Type parameters are optional.  The return type annotation is required (though the type
   * itself may be [[Type.Inferred]] if the parser accepts it via [[parseType]]).
   */
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

  /**
   * Parses an `if` conditional expression.
   *
   * Syntax: `if cond then ifTrue [else ifFalse]`
   *
   * When no `else` branch is present, the missing branch is filled with `VarCall("Unit")`.
   */
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

  /**
   * Parses a `for` loop.
   *
   * Syntax: `for iterator in iterable do body`
   */
  lazy val parseFor: Expr < Parse[Token] =
    Parse.inOrder(
      Parse.literal(Token.For),
      Parse.require(parseIdentifier),
      Parse.require(Parse.literal(Token.In)),
      Parse.require(parseBlockOrExpr),
      Parse.require(Parse.literal(Token.Do)),
      Parse.require(parseBlockOrExpr)
    ).map((_, id, _, range, _, body) => Expr.For(id, range, body))

  /**
   * Parses a `while` loop.
   *
   * Syntax: `while cond do body`
   */
  lazy val parseWhile: Expr < Parse[Token] =
    Parse.inOrder(
      Parse.literal(Token.While),
      Parse.require(parseBlockOrExpr),
      Parse.require(Parse.literal(Token.Do)),
      Parse.require(parseBlockOrExpr)
    ).map((_, cond, _, body) => Expr.While(cond, body))

  /**
   * Parses a `class` definition.
   *
   * Syntax: `class Name [TypeParams] [(params)] [= body]`
   *
   * Both type parameters and constructor parameters are optional.  The body is also optional;
   * an empty class (no `=` clause) produces an empty body chunk.
   */
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

  /**
   * Parses a lambda expression: `(param: Type, …) => body`.
   *
   * The parameter list is required and may be empty.  The body must be an indented block
   * (parsed by [[parseBlockBody]]).
   *
   * Lambdas are desugared into an [[Expr.Block]] containing an anonymous [[Expr.FunDef]]
   * named `lambda` immediately followed by an [[Expr.VarCall]] that returns it.
   */
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

  /**
   * Parses an indented block body: a newline-separated sequence of expressions.
   *
   * Trailing newlines are permitted.  The resulting expressions are wrapped in [[Expr.Block]].
   */
  lazy val parseBlockBody: Expr.Block < Parse[Token] =
    Parse.separatedBy(
      parseExpr,
      Parse.literal(Token.Newline),
      allowTrailing = true
    ).map(Expr.Block.apply)

  /**
   * Parses an indented block (delimited by [[Token.Indent]] / [[Token.DeIndent]]).
   *
   * Wraps [[parseBlockBody]] between the synthetic layout tokens emitted by the [[Lexer]].
   */
  lazy val parseBlockOrExpr: Expr.Block < Parse[Token] =
    Parse.between(
      Parse.literal(Token.Indent),
      parseBlockBody,
      Parse.literal(Token.DeIndent)
    )

  /**
   * Parses any Algorab expression.
   *
   * Tries each of the declaration and control-flow forms before falling back to the
   * binary-operator chain.  Ordered so that more specific forms (assignments, definitions)
   * are attempted before the general expression parsers.
   */
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

  /**
   * Parses a complete Algorab program.
   *
   * A program is a block body optionally surrounded by leading/trailing [[Token.Newline]]s.
   * The entire token stream must be consumed; any remaining tokens result in a parse error.
   */
  val parseAst: Expr < Parse[Token] = Parse.entireInput(
    Parse.between(
      Parse.readWhile[Token](_ == Token.Newline),
      Parse.require(parseBlockBody),
      Parse.readWhile[Token](_ == Token.Newline)
    )
  )
