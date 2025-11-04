package org.algorab.typer

import kyo.*
import org.algorab.assertionError
import org.algorab.ast.Type
import org.algorab.ast.tpd
import org.algorab.ast.untpd
import scala.annotation.meta.param

object Typer:

  def assertSubtype(tpe: Type, expected: Type*): Unit < Typing = direct:
    if expected.exists(TypeContext.isSubtype(tpe, _).now) then ()
    else Typing.fail(TypeFailure.Mismatch(tpe, expected*)).now

  def assertExprType(expr: tpd.Expr, expected: Type*): Unit < Typing =
    assertSubtype(expr.exprType, expected*)

  def assertBinaryOp(left: untpd.Expr, right: untpd.Expr, expected: Type*)(op: (tpd.Expr, tpd.Expr) => tpd.Expr): tpd.Expr < Typing = direct:
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
  )(expected: (Type, Type)*)(op: (tpd.Expr, tpd.Expr, Type) => tpd.Expr): tpd.Expr < Typing = direct:
    val typedLeft = typeExpr(left).now
    val typedRight = typeExpr(right).now

    // val result = expected.collectFirst:
    // case ((expected, result))
    //     if TypeContext.isSubtype(typedLeft.exprType, expected).now
    //       && TypeContext.isSubtype(typedRight.exprType, expected).now => result

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

  def assertComparison(left: untpd.Expr, right: untpd.Expr)(op: (tpd.Expr, tpd.Expr, Type) => tpd.Expr): tpd.Expr < Typing =
    assertBinaryOp(left, right, Type.Int, Type.Float)(op(_, _, Type.Boolean))

  def assertBooleanOp(left: untpd.Expr, right: untpd.Expr)(op: (tpd.Expr, tpd.Expr, Type) => tpd.Expr): tpd.Expr < Typing =
    assertBinaryOp(left, right, Type.Boolean)(op(_, _, Type.Boolean))

  def typeAndAssert(expr: untpd.Expr, expected: Type*): tpd.Expr < Typing = direct:
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

  def typeExpr(expr: untpd.Expr): tpd.Expr < Typing = direct:
    expr match
      case untpd.Expr.LBool(value)   => tpd.Expr.LBool(value, Type.Boolean)
      case untpd.Expr.LInt(value)    => tpd.Expr.LInt(value, Type.Int)
      case untpd.Expr.LFloat(value)  => tpd.Expr.LFloat(value, Type.Float)
      case untpd.Expr.LChar(value)   => tpd.Expr.LChar(value, Type.Char)
      case untpd.Expr.LString(value) => tpd.Expr.LString(value, Type.String)
      case untpd.Expr.Not(expr)      => tpd.Expr.Not(typeAndAssert(expr, Type.Boolean).now, Type.Boolean)
      case untpd.Expr.Equal(left, right) =>
        val (typedLeft, typedRight) = typeAndAssertRelated(left, right).now
        tpd.Expr.Equal(typedLeft, typedRight, Type.Boolean)
      case untpd.Expr.NotEqual(left, right) =>
        val (typedLeft, typedRight) = typeAndAssertRelated(left, right).now
        tpd.Expr.NotEqual(typedLeft, typedRight, Type.Boolean)
      case untpd.Expr.Less(left, right) =>
        assertComparison(left, right)(tpd.Expr.Less.apply).now
      case untpd.Expr.LessEqual(left, right) =>
        assertComparison(left, right)(tpd.Expr.LessEqual.apply).now
      case untpd.Expr.Greater(left, right) =>
        assertComparison(left, right)(tpd.Expr.Greater.apply).now
      case untpd.Expr.GreaterEqual(left, right) =>
        assertComparison(left, right)(tpd.Expr.GreaterEqual.apply).now
      case untpd.Expr.Plus(expr) =>
        typeAndAssert(expr, Type.Int, Type.Float).now
      case untpd.Expr.Minus(expr) =>
        val typedExpr = typeExpr(expr).now
        typedExpr.exprType match
          case Type.Int   => tpd.Expr.Minus(typedExpr, Type.Int)
          case Type.Float => tpd.Expr.Minus(typedExpr, Type.Float)
          case _          => Typing.failAndAbort(TypeFailure.Mismatch(typedExpr.exprType, Type.Int, Type.Float)).now

      case untpd.Expr.Add(left, right) =>
        assertDependentBinaryOp(left, right)(
          Type.Int -> Type.Int,
          Type.Float -> Type.Float
        )(tpd.Expr.Add.apply).now
      case untpd.Expr.Sub(left, right) =>
        assertDependentBinaryOp(left, right)(
          Type.Int -> Type.Int,
          Type.Float -> Type.Float
        )(tpd.Expr.Sub.apply).now
      case untpd.Expr.Mul(left, right) =>
        assertDependentBinaryOp(left, right)(
          Type.Int -> Type.Int,
          Type.Float -> Type.Float
        )(tpd.Expr.Mul.apply).now
      case untpd.Expr.Div(left, right) =>
        assertBinaryOp(left, right, Type.Int, Type.Float)(tpd.Expr.IntDiv(_, _, Type.Float)).now
      case untpd.Expr.IntDiv(left, right) =>
        assertBinaryOp(left, right, Type.Int, Type.Float)(tpd.Expr.IntDiv(_, _, Type.Int)).now
      case untpd.Expr.Mod(left, right) =>
        assertBinaryOp(left, right, Type.Int, Type.Float)(tpd.Expr.Mod(_, _, Type.Int)).now
      case untpd.Expr.And(left, right) =>
        assertBooleanOp(left, right)(tpd.Expr.And.apply).now
      case untpd.Expr.Or(left, right) =>
        assertBooleanOp(left, right)(tpd.Expr.Or.apply).now
      case untpd.Expr.VarCall(name) =>
        val varType = TypeContext.getVariableOrFail(name).now
        tpd.Expr.VarCall(name, varType)
      case untpd.Expr.ValDef(name, tpe, expr, mutable) =>
        val typedExpr = TypeContext.inNewScope(typeExpr(expr)).now
        
        if tpe == Type.Inferred then TypeContext.updateVariable(name, typedExpr.exprType).now
        else assertExprType(typedExpr, tpe).now

        tpd.Expr.ValDef(name, tpe.notInferredOr(typedExpr.exprType), typedExpr, mutable, Type.Unit)
      case untpd.Expr.Assign(name, expr) =>
        val typedExpr = typeExpr(expr).now
        val varType = TypeContext.getVariableOrFail(name).now
        Typing.abortIfFail(assertExprType(typedExpr, varType)).now
        tpd.Expr.Assign(name, typedExpr, Type.Unit)
      case untpd.Expr.Apply(expr, args) =>
        val typedExpr = typeExpr(expr).now
        val typedArgs = args.map(typeExpr(_).now)
        typedExpr.exprType match
          case Type.Fun(params, output) =>
            typedArgs.zip(params).foreach(assertExprType(_, _).now)

            tpd.Expr.Apply(typedExpr, typedArgs, output)

          case tpe =>
            Typing.failAndAbort(TypeFailure.Mismatch(tpe, Type.Fun(typedArgs.map(_.exprType), Type.Inferred))).now
      case untpd.Expr.FunDef(name, typeParams, params, retType, body) =>
        val paramTypes = params.map(_._2)
        val funcType = Type.Fun(paramTypes, retType)

        TypeContext.inNewScope:
          direct:
            params.foreach((paramName, paramType) =>
              TypeContext.declareVariable(paramName, paramType).now
            )

            val typedBody = typeExpr(body).now

            if retType == Type.Inferred then
              TypeContext.updateVariable(name, Type.Fun(paramTypes, typedBody.exprType)).now
            else
              assertExprType(typedBody, retType).now
            
            tpd.Expr.FunDef(name, typeParams, params, retType, typedBody, funcType)
        .now
        
      case untpd.Expr.Block(expressions) =>
        expressions.foreach:
          case untpd.Expr.ValDef(name, tpe, _, _) => 
            TypeContext.declareVariable(name, tpe).now
          case untpd.Expr.FunDef(name, typeParams, params, retType, _) =>
            val paramTypes = params.map(_._2)
            val funcType = Type.Fun(paramTypes, retType)
            TypeContext.declareVariable(name, funcType).now
          case _ =>

        val typedExprs = expressions.map(typeExpr(_).now)
        val blockType = if typedExprs.isEmpty then Type.Unit else typedExprs.last.exprType
        tpd.Expr.Block(typedExprs, blockType)
      case untpd.Expr.If(cond, ifTrue, ifFalse) =>
        val typedCond = typeExpr(cond).now
        assertExprType(typedCond, Type.Boolean).now
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
        assertExprType(typedCond, Type.Boolean).now
        val typedBody = TypeContext.inNewScope(typeExpr(body)).now
        tpd.Expr.While(typedCond, typedBody, Type.Unit)
      case untpd.Expr.For(iterator, iterable, body) =>
        val typedIterable = typeExpr(iterable).now
        typedIterable.exprType match
          case Type.Apply(Type.Array, Chunk(elemType)) =>
            TypeContext.inNewScope:
              direct:
                TypeContext.declareVariable(iterator, elemType).now
                val typedBody = Var.use[TypeContext](iterContext => Var.set(iterContext).flatMap(_ => typeExpr(body))).now
                tpd.Expr.For(iterator, typedIterable, typedBody, Type.Unit)
            .now
          case tpe =>
            Typing.failAndAbort(TypeFailure.Mismatch(tpe, Type.Array)).now
