package org.algorab.typer

import kyo.*
import org.algorab.assertionError
import org.algorab.ast.Identifier
import org.algorab.ast.tpd
import org.algorab.ast.untpd
import scala.annotation.meta.param

object Typer:

  def assertSubtype(tpe: tpd.Type, expected: tpd.Type*): Unit < Typing = direct:
    if expected.exists(TypeContext.isSubtype(tpe, _).now) then ()
    else Typing.fail(TypeFailure.Mismatch(tpe, expected*)).now

  def assertExprType(expr: tpd.Expr, expected: tpd.Type*): Unit < Typing =
    assertSubtype(expr.exprType, expected*)

  def assertBinaryOp(left: untpd.Expr, right: untpd.Expr, expected: tpd.Type*)(op: (tpd.Expr, tpd.Expr) => tpd.Expr): tpd.Expr < Typing = direct:
    val typedLeft = typeExpr(left).now
    val typedRight = typeExpr(right).now

    Typing.abortIfFail(
      assertExprType(typedLeft, expected*)
        .andThen(assertExprType(typedRight, expected*))
    ).now

    op(typedLeft, typedRight)

  def assertDependentBinaryOp(
      left: untpd.Expr,
      right: untpd.Expr
  )(expected: (tpd.Type, tpd.Type)*)(op: (tpd.Expr, tpd.Expr, tpd.Type) => tpd.Expr): tpd.Expr < Typing = direct:
    val typedLeft = typeExpr(left).now
    val typedRight = typeExpr(right).now

    val result = Kyo.findFirst(expected)(tpl =>
      for
        leftOk <- TypeContext.isSubtype(typedLeft.exprType, tpl._1)
        rightOk <- TypeContext.isSubtype(typedRight.exprType, tpl._1)
      yield
        if leftOk && rightOk then Present(tpl._2) else Absent
    ).now

    result match
      case Present(resultType) => op(typedLeft, typedRight, resultType)
      case Absent =>
        val operandTypes = typedLeft.exprType.zip(typedRight.exprType)
        val expectedTypes = expected.map(tpe => tpe._1.zip(tpe._1))
        Typing.failAndAbort(TypeFailure.Mismatch(operandTypes, expectedTypes*)).now

  def assertComparison(left: untpd.Expr, right: untpd.Expr)(op: (tpd.Expr, tpd.Expr, tpd.Type) => tpd.Expr): tpd.Expr < Typing =
    assertBinaryOp(left, right, tpd.Type.Int, tpd.Type.Float)(op(_, _, tpd.Type.Boolean))

  def assertBooleanOp(left: untpd.Expr, right: untpd.Expr)(op: (tpd.Expr, tpd.Expr, tpd.Type) => tpd.Expr): tpd.Expr < Typing =
    assertBinaryOp(left, right, tpd.Type.Boolean)(op(_, _, tpd.Type.Boolean))

  def typeAndAssert(expr: untpd.Expr, expected: tpd.Type*): tpd.Expr < Typing = direct:
    val typedExpr = typeExpr(expr).now
    Typing.abortIfFail(assertExprType(typedExpr, expected*)).now
    typedExpr

  def typeAndAssertRelated(exprA: untpd.Expr, exprB: untpd.Expr): (tpd.Expr, tpd.Expr) < Typing = direct:
    val typedA = typeExpr(exprA).now
    val typedB = typeExpr(exprB).now
    if TypeContext.isSubtype(typedA.exprType, typedB.exprType).now
      || TypeContext.isSubtype(typedB.exprType, typedA.exprType).now
    then (typedA, typedB)
    else Typing.failAndAbort(TypeFailure.Mismatch(typedA.exprType, typedB.exprType)).now

  def declareTypeParamsAndResolveFunTypes(funDef: untpd.Expr.FunDef): (
    Chunk[(Identifier, Identifier)],
    Chunk[(Identifier, tpd.Type)],
    Chunk[tpd.Type],
    tpd.Type,
    tpd.Type
  ) < Typing = direct:
    val uniqueTypeParams = funDef.typeParams.map(tp => (tp, TypeContext.newUniqueTypeName(tp).now))
    uniqueTypeParams.foreach((originalName, newName) =>
      TypeContext.declareType(originalName, tpd.Type.Generic(newName)).now
    )

    val resolvedParams = funDef.params.map((name, tpe) => (name, resolveType(tpe).now))
    val paramTypes = resolvedParams.map(_._2)
    val resolvedRetType = resolveType(funDef.retType).now

    if funDef.typeParams.isEmpty then (uniqueTypeParams, resolvedParams, paramTypes, resolvedRetType, tpd.Type.Fun(paramTypes, resolvedRetType))
    else (uniqueTypeParams, resolvedParams, paramTypes, resolvedRetType, tpd.Type.TypeFun(uniqueTypeParams.map(_._2), tpd.Type.Fun(paramTypes, resolvedRetType)))

  def resolveType(tpe: untpd.Type): tpd.Type < Typing = direct:
    tpe match
      case untpd.Type.Inferred => tpd.Type.Inferred
      case untpd.Type.Ref(name) => TypeContext.getType(name).now match
        case Some(resolvedType) => resolvedType
        case None => Typing.failAndAbort(TypeFailure.UnknownType(name)).now
      case untpd.Type.Apply(base, args) =>
        tpd.Type.Apply(resolveType(base).now, args.map(resolveType(_).now))
      case untpd.Type.Fun(params, output) =>
        tpd.Type.Fun(params.map(resolveType(_).now), resolveType(output).now)
      case untpd.Type.TypeFun(typeParams, output) =>
        tpd.Type.TypeFun(typeParams, resolveType(output).now)
      case untpd.Type.Tuple(elements) =>
        tpd.Type.Tuple(elements.map(resolveType(_).now))
  
  def typeExpr(expr: untpd.Expr): tpd.Expr < Typing = direct:
    expr match
      case untpd.Expr.LBool(value)   => tpd.Expr.LBool(value, tpd.Type.Boolean)
      case untpd.Expr.LInt(value)    => tpd.Expr.LInt(value, tpd.Type.Int)
      case untpd.Expr.LFloat(value)  => tpd.Expr.LFloat(value, tpd.Type.Float)
      case untpd.Expr.LChar(value)   => tpd.Expr.LChar(value, tpd.Type.Char)
      case untpd.Expr.LString(value) => tpd.Expr.LString(value, tpd.Type.String)
      case untpd.Expr.Not(expr)      => tpd.Expr.Not(typeAndAssert(expr, tpd.Type.Boolean).now, tpd.Type.Boolean)
      case untpd.Expr.Equal(left, right) =>
        val (typedLeft, typedRight) = typeAndAssertRelated(left, right).now
        tpd.Expr.Equal(typedLeft, typedRight, tpd.Type.Boolean)
      case untpd.Expr.NotEqual(left, right) =>
        val (typedLeft, typedRight) = typeAndAssertRelated(left, right).now
        tpd.Expr.NotEqual(typedLeft, typedRight, tpd.Type.Boolean)
      case untpd.Expr.Less(left, right) =>
        assertComparison(left, right)(tpd.Expr.Less.apply).now
      case untpd.Expr.LessEqual(left, right) =>
        assertComparison(left, right)(tpd.Expr.LessEqual.apply).now
      case untpd.Expr.Greater(left, right) =>
        assertComparison(left, right)(tpd.Expr.Greater.apply).now
      case untpd.Expr.GreaterEqual(left, right) =>
        assertComparison(left, right)(tpd.Expr.GreaterEqual.apply).now
      case untpd.Expr.Plus(expr) =>
        typeAndAssert(expr, tpd.Type.Int, tpd.Type.Float).now
      case untpd.Expr.Minus(expr) =>
        val typedExpr = typeExpr(expr).now
        typedExpr.exprType match
          case tpd.Type.Int   => tpd.Expr.Minus(typedExpr, tpd.Type.Int)
          case tpd.Type.Float => tpd.Expr.Minus(typedExpr, tpd.Type.Float)
          case _          => Typing.failAndAbort(TypeFailure.Mismatch(typedExpr.exprType, tpd.Type.Int, tpd.Type.Float)).now

      case untpd.Expr.Add(left, right) =>
        assertDependentBinaryOp(left, right)(
          tpd.Type.Int -> tpd.Type.Int,
          tpd.Type.Float -> tpd.Type.Float
        )(tpd.Expr.Add.apply).now
      case untpd.Expr.Sub(left, right) =>
        assertDependentBinaryOp(left, right)(
          tpd.Type.Int -> tpd.Type.Int,
          tpd.Type.Float -> tpd.Type.Float
        )(tpd.Expr.Sub.apply).now
      case untpd.Expr.Mul(left, right) =>
        assertDependentBinaryOp(left, right)(
          tpd.Type.Int -> tpd.Type.Int,
          tpd.Type.Float -> tpd.Type.Float
        )(tpd.Expr.Mul.apply).now
      case untpd.Expr.Div(left, right) =>
        assertBinaryOp(left, right, tpd.Type.Int, tpd.Type.Float)(tpd.Expr.IntDiv(_, _, tpd.Type.Float)).now
      case untpd.Expr.IntDiv(left, right) =>
        assertBinaryOp(left, right, tpd.Type.Int, tpd.Type.Float)(tpd.Expr.IntDiv(_, _, tpd.Type.Int)).now
      case untpd.Expr.Mod(left, right) =>
        assertBinaryOp(left, right, tpd.Type.Int, tpd.Type.Float)(tpd.Expr.Mod(_, _, tpd.Type.Int)).now
      case untpd.Expr.And(left, right) =>
        assertBooleanOp(left, right)(tpd.Expr.And.apply).now
      case untpd.Expr.Or(left, right) =>
        assertBooleanOp(left, right)(tpd.Expr.Or.apply).now
      case untpd.Expr.VarCall(name) =>
        val varType = TypeContext.getVariableOrFail(name).now.tpe
        tpd.Expr.VarCall(name, varType)
      case untpd.Expr.ValDef(name, tpe, expr, mutable) =>
        val resolvedType = resolveType(tpe).now
        val typedExpr = TypeContext.inNewScope(typeExpr(expr)).now
        
        if resolvedType == tpd.Type.Inferred then TypeContext.updateVariable(name, Variable(typedExpr.exprType, mutable)).now
        else assertExprType(typedExpr, resolvedType).now

        tpd.Expr.ValDef(name, resolvedType.notInferredOr(typedExpr.exprType), typedExpr, mutable, tpd.Type.Unit)
      case untpd.Expr.Assign(name, expr) =>
        val typedExpr = typeExpr(expr).now
        val variable = TypeContext.getVariableOrFail(name).now
        if variable.mutable then
          Typing.abortIfFail(assertExprType(typedExpr, variable.tpe)).now
          tpd.Expr.Assign(name, typedExpr, tpd.Type.Unit)
        else
          Typing.failAndAbort(TypeFailure.ImmutableVariableAssignment(name)).now
      case untpd.Expr.Apply(expr, args) =>
        val typedExpr = typeExpr(expr).now
        val typedArgs = args.map(typeExpr(_).now)
        typedExpr.exprType match
          case tpd.Type.Fun(params, output) =>
            typedArgs.zip(params).foreach(assertExprType(_, _).now)
            tpd.Expr.Apply(typedExpr, typedArgs, output)

          case tpd.Type.TypeFun(typeParams, funType@tpd.Type.Fun(params, output)) =>
            val resolvedTypes = params
              .zip(typedArgs)
              .collect:
                case (tpd.Type.Generic(name), arg) if typeParams.contains(name) => (name, arg.exprType)
              .groupMap(_._1)(_._2)
              .map((typeParam, types) =>
                (typeParam, Kyo.foldLeft(types)(tpd.Type.Nothing)(TypeContext.union).now))

            val replacements = resolvedTypes.toMap.withDefaultValue(tpd.Type.Nothing)

            tpd.Expr.Apply(
              typedExpr.withType(funType.replaceGeneric(replacements)),
              typedArgs,
              output.replaceGeneric(replacements).now
            )
          case tpe =>
            Typing.failAndAbort(TypeFailure.Mismatch(tpe, tpd.Type.Fun(typedArgs.map(_.exprType), tpd.Type.Inferred))).now
      case untpd.Expr.TypeApply(expr, types) =>
        val typedExpr = typeExpr(expr).now
        typedExpr.exprType match
          case tpd.Type.TypeFun(typeParams, output) =>
            val sizeCompare = types.sizeCompare(typeParams)
            if sizeCompare < 0 then
              Typing.failAndAbort(TypeFailure.MissingTypeArguments(typeParams.take(types.size))).now
            else if sizeCompare > 0 then
              Typing.failAndAbort(TypeFailure.TooManyTypeArguments(types, typeParams)).now
            else
              TypeContext.inNewScope:
                direct:
                  val replacements = typeParams
                    .zip(types)
                    .map((paramName, tpe) => (paramName, resolveType(tpe).now))
                    .toMap
                  
                  typedExpr.withType(output.replaceGeneric(replacements).now)
              .now
          case tpe => Typing.failAndAbort(TypeFailure.Mismatch(tpe, tpd.Type.TypeFun(Chunk.empty, tpd.Type.Inferred))).now
      case funDef@untpd.Expr.FunDef(name, typeParams, params, retType, body) =>
        TypeContext.inNewScope:
          direct:
            val (uniqueTypeParams, resolvedParams, paramTypes, resolvedRetType, _) = declareTypeParamsAndResolveFunTypes(funDef).now

            TypeContext.inNewScope:
              direct:
                resolvedParams.foreach((name, tpe) => TypeContext.declareVariable(name, Variable(tpe,false)).now)

                val typedBody = typeExpr(body).now

                if resolvedRetType == tpd.Type.Inferred then
                  val inferredType =
                    if typeParams.isEmpty then tpd.Type.Fun(paramTypes, typedBody.exprType)
                    else tpd.Type.TypeFun(uniqueTypeParams.map(_._2), tpd.Type.Fun(paramTypes, typedBody.exprType))
                  TypeContext.updateVariable(name, Variable(inferredType,false)).now
                else
                  assertExprType(typedBody, resolvedRetType).now
                
                tpd.Expr.FunDef(name, resolvedParams, resolvedRetType, typedBody, tpd.Type.Unit)
            .now
        .now
        
      case untpd.Expr.Block(expressions) =>
        expressions.foreach:
          case untpd.Expr.ValDef(name, tpe, _, mutable) => 
            val resolvedType = resolveType(tpe).now
            TypeContext.declareVariable(name, Variable(resolvedType, mutable)).now
          case funDef: untpd.Expr.FunDef =>
            val (_, _, _, _, funType) = TypeContext.inNewScope(declareTypeParamsAndResolveFunTypes(funDef)).now
            TypeContext.declareVariable(funDef.name, Variable(funType, false)).now
          case _ =>

        val typedExprs = expressions.map(typeExpr(_).now)
        val blockType = if typedExprs.isEmpty then tpd.Type.Unit else typedExprs.last.exprType
        tpd.Expr.Block(typedExprs, blockType)
      case untpd.Expr.If(cond, ifTrue, ifFalse) =>
        val typedCond = typeExpr(cond).now
        assertExprType(typedCond, tpd.Type.Boolean).now
        val typedIfTrue = TypeContext.inNewScope(typeExpr(ifTrue)).now
        val typedIfFalse = TypeContext.inNewScope(typeExpr(ifFalse)).now

        //TODO support inheritance
        if TypeContext.isSubtype(typedIfTrue.exprType, typedIfFalse.exprType).now then
          tpd.Expr.If(typedCond, typedIfTrue, typedIfFalse, typedIfFalse.exprType)
        else if TypeContext.isSubtype(typedIfFalse.exprType, typedIfTrue.exprType).now then
          tpd.Expr.If(typedCond, typedIfTrue, typedIfFalse, typedIfTrue.exprType)
        else
          Typing.failAndAbort(
            TypeFailure.Mismatch(
              typedIfTrue.exprType.zip(typedIfFalse.exprType),
              typedIfTrue.exprType.zip(typedIfTrue.exprType)
            )
          ).now
      case untpd.Expr.While(cond, body) =>
        val typedCond = typeExpr(cond).now
        assertExprType(typedCond, tpd.Type.Boolean).now
        val typedBody = TypeContext.inNewScope(typeExpr(body)).now
        tpd.Expr.While(typedCond, typedBody, tpd.Type.Unit)
      case untpd.Expr.For(iterator, iterable, body) =>
        val typedIterable = typeExpr(iterable).now
        typedIterable.exprType match
          case tpd.Type.Apply(tpd.Type.Array, Chunk(elemType)) =>
            TypeContext.inNewScope:
              direct:
                TypeContext.declareVariable(iterator, Variable(elemType,false)).now
                val typedBody = Var.use[TypeContext](iterContext => Var.set(iterContext).flatMap(_ => typeExpr(body))).now
                
                //TODO translate to while
                tpd.Expr.While(???, ???, tpd.Type.Unit)
            .now
          case tpe =>
            Typing.failAndAbort(TypeFailure.Mismatch(tpe, tpd.Type.Array)).now
