package org.algorab.typing

import org.algorab.AlgorabProgram
import org.algorab.ast.Symbol
import org.algorab.ast.Symbol.Root.position
import org.algorab.ast.SymbolId
import org.algorab.ast.raw.Statement
import org.algorab.ast.resolved
import org.algorab.ast.typed
import org.algorab.resolution.ResolutionContext
import purelogic.*

/**
 * The typing phase.
 */
object Typer:

  /**
   * Resolve a type to its fully typed, concrete form.
   *
   * @param tpe the type to resolve
   * @return the given type with fully concrete references aka no generic or inferred
   */
  def resolveType(tpe: resolved.Type): typed.Type = tpe match
    case resolved.Type.Ref(symbol) => typed.Type.Class(symbol)
    case resolved.Type.Inferred    => throw AssertionError("Resolving Inferred type")

  /**
   * Unify two types.
   *
   * @param typeA the first type
   * @param typeB the second type
   * @return the type unifying the two given ones, usually the lowest common ancestor
   */
  def unify(typeA: typed.Type, typeB: typed.Type): Typing[typed.Type] =
    if typeA == typed.Type.Unit || typeB == typed.Type.Unit then typed.Type.Unit
    else if typeA == typed.Type.Invalid || typeB == typed.Type.Invalid then typed.Type.Invalid
    else if typeA == typed.Type.Int && typeB == typed.Type.Float then typed.Type.Float
    else if typeA == typed.Type.Float && typeB == typed.Type.Int then typed.Type.Float
    else if TypeContext.isSubtype(typeA, typeB) then typeB
    else if TypeContext.isSubtype(typeB, typeA) then typeA
    else typed.Type.Any

  /**
   * Type an expression and cast it to a certain type.
   * A type mismatch occurs if the expression's type cannot be casted to the expected one.
   *
   * @param expr the expression to type
   * @param to the target type
   * @return the given expression, typed to the given type
   */
  def typeExprTo(expr: resolved.Expr, to: typed.Type): Typing[typed.Expr] = castExpr(typeExpr(expr), to)

  /**
   * Cast a typed expression to a certain type.
   * A type mismatch occurs if the expression's type cannot be casted to the expected one.
   *
   * @param expr the expression to cast
   * @param to the target type
   * @return the given expression, casted to the given type
   */
  def castExpr(expr: typed.Expr, to: typed.Type): Typing[typed.Expr] =
    if expr.tpe == typed.Type.Int && to == typed.Type.Float then
      typed.Expr.ToFloat(expr)
    else
      if !TypeContext.isSubtype(expr.tpe, to) then write(TypeError.simpleMismatch(List(to), expr.tpe, expr.position))
      expr

  /**
   * Ensure the given expression has a numeric type.
   *
   * @param expr the expression to check
   * @return the same expression, for better UX where it's used
   */
  def assertNumeric(expr: typed.Expr): Typing[typed.Expr] =
    if expr.tpe != typed.Type.Int && expr.tpe != typed.Type.Float then
      write(TypeError.simpleMismatch(List(typed.Type.Int, typed.Type.Float), expr.tpe, position))
    expr

  /**
   * Type a numeric binary operation.
   *
   * @param left the operation's LHS
   * @param right the operation's RHS
   * @param opText the textual representation of the operator such as "+"
   * @param op the typed operation's constructor
   * @return the typed binary operation
   */
  def typeBinaryNumOp(
      left: resolved.Expr,
      right: resolved.Expr,
      opText: String,
      op: (typed.Expr, typed.Expr, typed.Type) => typed.Expr
  ): Typing[typed.Expr] =
    val typedLeft = typeExpr(left)
    val typedRight = typeExpr(right)

    (typedLeft.tpe, typedRight.tpe) match
      case (typed.Type.Int, typed.Type.Int)     => op(typedLeft, typedRight, typed.Type.Int)
      case (typed.Type.Int, typed.Type.Float)   => op(typed.Expr.ToFloat(typedLeft), typedRight, typed.Type.Float)
      case (typed.Type.Float, typed.Type.Int)   => op(typedLeft, typed.Expr.ToFloat(typedRight), typed.Type.Float)
      case (typed.Type.Float, typed.Type.Float) => op(typedLeft, typedRight, typed.Type.Float)
      case (leftType, rightType) =>
        write(TypeError.Mismatch(
          expected = List(
            TypePattern.BinaryOperator(
              TypePattern.Union(List(TypePattern.Type(typed.Type.Int), TypePattern.Type(typed.Type.Float))),
              TypePattern.Union(List(TypePattern.Type(typed.Type.Int), TypePattern.Type(typed.Type.Float))),
              opText
            )
          ),
          got = List(leftType, rightType),
          position = position
        ))
        op(typedLeft, typedRight, typed.Type.Invalid)

  /**
   * Type a program.
   *
   * @param program the program to type
   * @return a typed representation of the given program
   */
  def typeProgram(program: resolved.Program): Typing[typed.Program] = program match
    case resolved.Program.Script(statements)         => typed.Program.Script(statements.map(typeStatement))
    case resolved.Program.Module(owner, definitions) => typed.Program.Module(owner, definitions.map(typeDefinition))

  /**
   * Type a statement.
   *
   * @param statement the program to type
   * @return a typed representation of the given statement
   */
  def typeStatement(statement: resolved.Statement): Typing[typed.Statement] = statement match
    case definition: resolved.Definition => typeDefinition(definition)
    case expr: resolved.Expr             => typeExpr(expr)

  def resolveDefinitionType(symbol: SymbolId): Typing[typed.Type] =
    if TypeContext.isTyped(symbol) then TypeContext.getType(symbol)
    else
      val definitionType = TypeContext.getDeclaration(symbol) match
        case resolved.Definition.Val(symbol, tpe, expr, mutable, position) =>
          if tpe == resolved.Type.Inferred then
            TypeContext.startInferring(symbol)
            val typedExpr = typeExpr(expr)
            typedExpr.tpe
          else
            resolveType(tpe)

        case resolved.Definition.Function(symbol, params, retType, body, position) =>
          val resolvedParams = params.map((sym, tpe) => (sym, resolveType(tpe)))
          val resolvedRetType = resolveType(retType)
          resolvedParams.foreach(TypeContext.assignType)
          typed.Type.Function(resolvedParams.map(_._2), resolvedRetType)

      TypeContext.assignType(symbol, definitionType)
      definitionType

  def typeDefinition(definition: resolved.Definition): Typing[typed.Definition] =
    val resolvedType = resolveDefinitionType(definition.symbol)
    definition match
      case resolved.Definition.Val(symbol, tpe, expr, mutable, position) =>
        typed.Definition.Val(symbol, resolvedType, typeExprTo(expr, resolvedType), mutable, position)
      case resolved.Definition.Function(symbol, params, retType, body, position) =>
        val resolvedParams = params.map(_._1).zip(resolvedType.asInstanceOf[typed.Type.Function].inputs)
        val resolvedRetType = resolvedType.asInstanceOf[typed.Type.Function].output
        typed.Definition.Function(symbol, resolvedParams, resolvedRetType, typeExprTo(body, resolvedRetType), position)

  def typeExpr(expr: resolved.Expr): Typing[typed.Expr] = expr match
    case resolved.Expr.LBool(value, position)          => typed.Expr.LBool(value, typed.Type.Boolean, position)
    case resolved.Expr.LInt(value, position)           => typed.Expr.LInt(value, typed.Type.Int, position)
    case resolved.Expr.LFloat(value, position)         => typed.Expr.LFloat(value, typed.Type.Float, position)
    case resolved.Expr.LChar(value, position)          => typed.Expr.LChar(value, typed.Type.Char, position)
    case resolved.Expr.LString(value, position)        => typed.Expr.LString(value, typed.Type.String, position)
    case resolved.Expr.Not(expr, position)             => typed.Expr.Not(typeExprTo(expr, typed.Type.Boolean), typed.Type.Boolean, position)
    case resolved.Expr.Equal(left, right, position)    => typed.Expr.Equal(typeExpr(left), typeExpr(right), typed.Type.Boolean, position)
    case resolved.Expr.NotEqual(left, right, position) => typed.Expr.NotEqual(typeExpr(left), typeExpr(right), typed.Type.Boolean, position)
    case resolved.Expr.Less(left, right, position) =>
      typed.Expr.Less(assertNumeric(typeExpr(left)), assertNumeric(typeExpr(right)), typed.Type.Boolean, position)
    case resolved.Expr.LessEqual(left, right, position) =>
      typed.Expr.LessEqual(assertNumeric(typeExpr(left)), assertNumeric(typeExpr(right)), typed.Type.Boolean, position)
    case resolved.Expr.Greater(left, right, position) =>
      typed.Expr.Greater(assertNumeric(typeExpr(left)), assertNumeric(typeExpr(right)), typed.Type.Boolean, position)
    case resolved.Expr.GreaterEqual(left, right, position) =>
      typed.Expr.GreaterEqual(assertNumeric(typeExpr(left)), assertNumeric(typeExpr(right)), typed.Type.Boolean, position)
    case resolved.Expr.Plus(expr, position) =>
      val typedExpr = typeExpr(expr)
      typed.Expr.Plus(assertNumeric(typedExpr), typedExpr.tpe, position)
    case resolved.Expr.Minus(expr, position) =>
      val typedExpr = typeExpr(expr)
      typed.Expr.Minus(assertNumeric(typedExpr), typedExpr.tpe, position)
    case resolved.Expr.Add(left, right, position)    => typeBinaryNumOp(left, right, "+", typed.Expr.Add(_, _, _, position))
    case resolved.Expr.Sub(left, right, position)    => typeBinaryNumOp(left, right, "-", typed.Expr.Sub(_, _, _, position))
    case resolved.Expr.Mul(left, right, position)    => typeBinaryNumOp(left, right, "*", typed.Expr.Mul(_, _, _, position))
    case resolved.Expr.Div(left, right, position)    => typeBinaryNumOp(left, right, "/", typed.Expr.Div(_, _, _, position))
    case resolved.Expr.IntDiv(left, right, position) => typeBinaryNumOp(left, right, "//", typed.Expr.IntDiv(_, _, _, position))
    case resolved.Expr.Mod(left, right, position)    => typeBinaryNumOp(left, right, "+", typed.Expr.Mod(_, _, _, position))
    case resolved.Expr.And(left, right, position) =>
      typed.Expr.And(typeExprTo(left, typed.Type.Boolean), typeExprTo(right, typed.Type.Boolean), typed.Type.Boolean, position)
    case resolved.Expr.Or(left, right, position) =>
      typed.Expr.Or(typeExprTo(left, typed.Type.Boolean), typeExprTo(right, typed.Type.Boolean), typed.Type.Boolean, position)
    case resolved.Expr.VarCall(symbol, position) => typed.Expr.VarCall(symbol, resolveDefinitionType(symbol), position)
    case resolved.Expr.Assign(symbol, expr, position) =>
      typed.Expr.Assign(symbol, typeExprTo(expr, TypeContext.getType(symbol)), typed.Type.Unit, position)
    case resolved.Expr.Select(expr, member, position) =>
      write(TypeError.UnsupportedOOP(position))
      typed.Expr.Invalid(typed.Type.Invalid, position)

    case resolved.Expr.Apply(expr, args, position) =>
      val typedExpr = typeExpr(expr)
      val typedArgs = args.map(typeExpr)
      typedExpr.tpe match
        case typed.Type.Function(inputs, output) =>
          if inputs.sizeCompare(typedArgs) != 0 then write(TypeError.ApplyMismatch(inputs, typedArgs.map(_.tpe), position))
          typed.Expr.Apply(typedExpr, typedArgs.zip(inputs).map(castExpr), output, position)
        case _ =>
          if typedExpr.tpe != typed.Type.Invalid then write(TypeError.ApplyOnNonFunction(typedExpr.tpe, position))
          typed.Expr.Apply(typedExpr, typedArgs, typed.Type.Invalid, position)

    case resolved.Expr.Block(statements, position) =>
      val typedStatements = statements.map(typeStatement)
      val blockType = typedStatements.lastOption match
        case Some(expr: typed.Expr) => expr.tpe
        case _                      => typed.Type.Unit

      typed.Expr.Block(typedStatements, blockType, position)
    case resolved.Expr.If(cond, ifTrue, ifFalse, position) =>
      val typedCond = typeExprTo(cond, typed.Type.Boolean)
      val typedIfTrue = typeExpr(ifTrue)
      val typedIfFalse = typeExpr(ifFalse)
      val ifType = unify(typedIfTrue.tpe, typedIfFalse.tpe)

      typed.Expr.If(typedCond, castExpr(typedIfTrue, ifType), castExpr(typedIfFalse, ifType), ifType, position)

    case resolved.Expr.While(cond, body, position) =>
      typed.Expr.While(typeExprTo(cond, typed.Type.Boolean), typeExpr(body), typed.Type.Unit, position)
    case resolved.Expr.For(iterator, iterable, body, position) =>
      TypeContext.assignType(iterator, typed.Type.Any)
      typed.Expr.For(iterator, typeExpr(iterable), typeExpr(body), typed.Type.Unit, position)
    case resolved.Expr.Invalid(position) => typed.Expr.Invalid(typed.Type.Invalid, position)

  /**
   * Type the given programs.
   *
   * @param symbols the declared symbols
   * @param declarations the declaration of each user-defined symbol
   * @param programs the name-resolved programs to type
   * @return the typed programs
   */
  def apply(
      symbols: Map[SymbolId, Symbol],
      declarations: Map[SymbolId, resolved.Definition]
  )(programs: Seq[resolved.Program]): AlgorabProgram[Seq[typed.Program]] =
    Typing(symbols, declarations)(programs.map(typeProgram))
