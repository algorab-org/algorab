package org.algorab.typing

import org.algorab.AlgorabProgram
import org.algorab.ast.Symbol
import org.algorab.ast.SymbolId
import org.algorab.ast.raw.Statement
import org.algorab.ast.resolved
import org.algorab.ast.typed
import org.algorab.resolution.ResolutionContext
import purelogic.*
import org.algorab.ast.Symbol.Root.position
import scala.annotation.tailrec
import org.algorab.util.SourcePosition

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
    case resolved.Type.TypeFunction(params, output) => typed.Type.TypeFunction(params, resolveType(output))
    case resolved.Type.Inferred    => throw AssertionError("Resolving Inferred type")

  def resolveGenerics(tpe: typed.Type, args: Map[SymbolId, typed.Type]): typed.Type = tpe match
    case typed.Type.Nothing => typed.Type.Nothing
    case typed.Type.Class(symbol) => args.getOrElse(symbol, tpe)
    case typed.Type.Function(inputs, output) => typed.Type.Function(inputs.map(resolveGenerics(_, args)), resolveGenerics(output, args))
    case typed.Type.TypeFunction(params, output) => typed.Type.TypeFunction(params, resolveGenerics(output, args))
    case typed.Type.Apply(tpe, typeArgs) => typed.Type.Apply(resolveGenerics(tpe, args), typeArgs.map(resolveGenerics(_, args)))
    case typed.Type.Invalid => typed.Type.Invalid

  def inferGenerics(
    params: List[SymbolId],
    inputs: List[typed.Type],
    args: List[typed.Expr],
    output: typed.Type,
    expected: Option[typed.Type]
  ): Typing[Map[SymbolId, typed.Type]] =
    val genericParams = params.toSet

    val fromArguments =
      inputs.zip(args).foldLeft(Map.empty[SymbolId, typed.Type]):
        case (substitutions, (formal, actual)) =>
          inferConstraint(formal, actual.tpe, genericParams, substitutions)

    expected match
      case Some(expectedType) =>
        inferConstraint(output, expectedType, genericParams, fromArguments)
      case None =>
        fromArguments

  def inferConstraint(
      formal: typed.Type,
      actual: typed.Type,
      genericParams: Set[SymbolId],
      substitutions: Map[SymbolId, typed.Type]
  ): Typing[Map[SymbolId, typed.Type]] =
    formal match
      case typed.Type.Class(symbol) if genericParams.contains(symbol) =>
        substitutions.get(symbol) match
          case None =>
            substitutions.updated(symbol, actual)

          case Some(previous) =>
            substitutions.updated(symbol, unify(previous, actual))

      case typed.Type.Function(formalInputs, formalOutput) =>
        actual match
          case typed.Type.Function(actualInputs, actualOutput) if formalInputs.size == actualInputs.size =>
            val withInputs =
              formalInputs.zip(actualInputs).foldLeft(substitutions):
                case (current, (formalInput, actualInput)) =>
                  inferConstraint(
                    formalInput,
                    actualInput,
                    genericParams,
                    current
                  )

            inferConstraint(
              formalOutput,
              actualOutput,
              genericParams,
              withInputs
            )

          case _ =>
            substitutions

      case typed.Type.TypeFunction(formalParams, formalOutput) =>
        actual match
          case typed.Type.TypeFunction(actualParams, actualOutput)
              if formalParams.size == actualParams.size =>
            inferConstraint(
              formalOutput,
              actualOutput,
              genericParams,
              substitutions
            )

          case _ =>
            substitutions

      case _ =>
        substitutions

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
  def typeExprTo(expr: resolved.Expr, to: typed.Type): Typing[typed.Expr] = expr match
      case resolved.Expr.If(cond, ifTrue, ifFalse, position) =>
        val typedCond = typeExprTo(cond, typed.Type.Boolean)
        val typedIfTrue = typeExprTo(ifTrue, to)
        val typedIfFalse = typeExprTo(ifFalse, to)

        typed.Expr.If(typedCond, typedIfTrue, typedIfFalse, to, position)

      case resolved.Expr.Block(statements :+ (last: resolved.Expr), position) =>
        val typedStatements = statements.map(typeStatement)
        val typedLast = typeExprTo(last, to)
        typed.Expr.Block(typedStatements :+ typedLast, to, position)

      //TODO unify bidir variance once classes are implemented
      case resolved.Expr.Array(exprs, position) => to match
        case typed.Type.Apply(typed.Type.Array, List(elementType)) =>
          val typedExprs = exprs.map(typeExprTo(_, elementType))
          typed.Expr.Array(typedExprs, elementType, position)
        
        case _ => castExpr(typeExpr(expr), to)

      case _ => castExpr(typeExpr(expr), to)

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
    if expr.tpe != typed.Type.Int && expr.tpe != typed.Type.Float && expr.tpe != typed.Type.Invalid then
      write(TypeError.simpleMismatch(List(typed.Type.Int, typed.Type.Float), expr.tpe, expr.position))
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
      case (_, typed.Type.Invalid) | (typed.Type.Invalid, _) => op(typedLeft, typedRight, typed.Type.Invalid)
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
          position = left.position.union(right.position)
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

  def typeParam(param: resolved.Param): Typing[typed.Param] = param match
    case resolved.Param.Simple(symbol, tpe, default, position) =>
      typed.Param.Simple(symbol, resolveType(tpe), default.map(typeExpr), position)
    case resolved.Param.Varargs(symbol, tpe, position) =>
      typed.Param.Varargs(symbol, resolveType(tpe), position)

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

        case resolved.Definition.Function(symbol, typeParams, params, retType, body, position) =>
          val typedParams = params.map(typeParam)
          val typedRetType = resolveType(retType)
          typedParams.foreach(param => TypeContext.assignType(param.symbol, param.usageType))
          
          val functionType = typed.Type.Function(typedParams.map(_.usageType), typedRetType)
          if typeParams.isEmpty then functionType
          else typed.Type.TypeFunction(typeParams, functionType)

      TypeContext.assignType(symbol, definitionType)
      definitionType

  def typeDefinition(definition: resolved.Definition): Typing[typed.Definition] =
    val resolvedType = resolveDefinitionType(definition.symbol)
    definition match
      case resolved.Definition.Val(symbol, tpe, expr, mutable, position) =>
        typed.Definition.Val(symbol, resolvedType, typeExprTo(expr, resolvedType), mutable, position)
      case resolved.Definition.Function(symbol, typeParams, params, retType, body, position) =>

        @tailrec
        def getFunctionType(tpe: typed.Type): (List[typed.Type], typed.Type) = tpe match
          case typed.Type.Function(inputs, output) => (inputs, output)
          case typed.Type.TypeFunction(params, output) => getFunctionType(output)
          case _ => throw AssertionError(s"Function definition with non-function type: $tpe")

        val (resolvedParams, resolvedRetType) = getFunctionType(resolvedType)
        typed.Definition.Function(
          symbol,
          params.map(_.symbol).zip(resolvedParams),
          resolvedRetType,
          typeExprTo(body, resolvedRetType),
          position
        )

  def desugarArguments(expr: resolved.Expr, args: List[resolved.Argument], position: SourcePosition): Typing[List[resolved.Expr]] =
    def getPositionalExprs(arguments: List[resolved.Argument]): List[resolved.Expr] =
      arguments.map:
        case resolved.Argument.Positional(_, true) =>
            write(TypeError.IllegalVariadic(position))
            resolved.Expr.Invalid(position)
          
        case resolved.Argument.Positional(expr, false) => expr

    def handleFunctionArguments(symbol: SymbolId): Typing[List[resolved.Expr]] =
      val params =
        get.declarations
          .get(symbol)
          .toList
          .flatMap(_.asInstanceOf[resolved.Definition.Function].params)

      params.lastOption match
        case Some(resolved.Param.Varargs(varargSymbol, _, _)) =>
          val (beforeVarargs, afterVarargs) = args match
            case before :+ last if last.variadic => (before, last.expr)
            case _ =>
              val (before, after) = args.splitAt(params.size - 1)
              val afterPosition = after.map(_.expr.position).reduceOption(_.union(_)).getOrElse(position)
              (before, resolved.Expr.Array(getPositionalExprs(after), afterPosition))

          getPositionalExprs(beforeVarargs) :+ afterVarargs

        case _ => getPositionalExprs(args)
    
    expr match
      case resolved.Expr.TypeApply(resolved.Expr.VarCall(symbol, _), _, _) if TypeContext.getSymbol(symbol).isInstanceOf[Symbol.Function] =>
        handleFunctionArguments(symbol)
      case resolved.Expr.VarCall(symbol, _) if TypeContext.getSymbol(symbol).isInstanceOf[Symbol.Function] =>
        handleFunctionArguments(symbol)

      case _ => getPositionalExprs(args)

  def typeApply(
    expr: resolved.Expr,
    args: List[resolved.Argument],
    position: SourcePosition
  ): Typing[typed.Expr] =
    val argExprs = desugarArguments(expr, args, position)
    val typedExpr = typeExpr(expr)
    typedExpr.tpe match
      case typed.Type.Function(inputs, output) =>
        if inputs.sizeCompare(argExprs) != 0 then
          write(TypeError.ApplyMismatch(inputs, argExprs.map(typeExpr(_).tpe), position))
          typed.Expr.Invalid(typed.Type.Invalid, position)
        else
          val typedArgs = argExprs.zip(inputs).map(typeExprTo)
          typed.Expr.Apply(typedExpr, typedArgs, output, position)

      case got =>
        write(TypeError.ApplyOnNonFunction(got, position))
        typed.Expr.Invalid(typed.Type.Invalid, position)

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

    case resolved.Expr.Array(exprs, position) =>
      val typedExprs = exprs.map(typeExpr)
      val elementType = typedExprs.foldLeft(typed.Type.Nothing)((tpe, expr) => unify(tpe, expr.tpe))
      typed.Expr.Array(typedExprs, elementType, position)
    case resolved.Expr.Apply(expr, args, position) => typeApply(expr, args, position)

    case resolved.Expr.TypeApply(expr, args, position) =>
      val typedExpr = typeExpr(expr)
      val argsType = args.map(resolveType)
      typedExpr.tpe match
        case typed.Type.TypeFunction(params, output) =>
          val typedArgs =
            if args.sizeCompare(params) != 0 then
              write(TypeError.TypeApplyMismatch(params, argsType, position))
              params.map((_, typed.Type.Invalid)).toMap
            else
              params.zip(argsType).toMap

          typedExpr.withType(resolveGenerics(output, typedArgs))

        case _ =>
          if typedExpr.tpe != typed.Type.Invalid then write(TypeError.TypeApplyOnNonFunction(typedExpr.tpe, position))
          typed.Expr.Invalid(typed.Type.Invalid, position)

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
