package org.algorab.typing

import org.algorab.ast.resolved
import org.algorab.ast.typed
import purelogic.*

def resolveType(tpe: resolved.Type): typed.Type = tpe match
  case resolved.Type.Ref(symbol) => typed.Type.Class(symbol)
  case resolved.Type.Inferred    => ???

def union(typeA: typed.Type, typeB: typed.Type): Typing[typed.Type] =
  if typeA == typed.Type.Unit || typeB == typed.Type.Unit then typed.Type.Unit
  else if typeA == typed.Type.Invalid || typeB == typed.Type.Invalid then typed.Type.Invalid
  else if typeA == typed.Type.Int && typeB == typed.Type.Float then typed.Type.Float
  else if typeA == typed.Type.Float && typeB == typed.Type.Int then typed.Type.Float
  else if TypeContext.isSubtype(typeA, typeB) then typeB
  else if TypeContext.isSubtype(typeB, typeA) then typeA
  else typed.Type.Any

def typeExprTo(expr: resolved.Expr, to: typed.Type): Typing[typed.Expr] = castExpr(typeExpr(expr), to)


// TODO int -> float cast
def castExpr(expr: typed.Expr, to: typed.Type): Typing[typed.Expr] =
  if TypeContext.isSubtype(expr.tpe, to) then write(TypeError.simpleMismatch(List(to), expr.tpe))
  expr

def assertNumeric(expr: typed.Expr): Typing[typed.Expr] =
  if expr.tpe != typed.Type.Int && expr.tpe != typed.Type.Float then write(TypeError.simpleMismatch(List(typed.Type.Int, typed.Type.Float), expr.tpe))
  expr

def typeBinaryNumOp(left: resolved.Expr, right: resolved.Expr, opText: String, op: (typed.Expr, typed.Expr, typed.Type) => typed.Expr): Typing[typed.Expr] =
  val typedLeft = typeExpr(left)
  val typedRight = typeExpr(right)

  (typedLeft.tpe, typedRight.tpe) match
    case (typed.Type.Int, typed.Type.Int) => op(typedLeft, typedRight, typed.Type.Int)
    case (typed.Type.Int, typed.Type.Float) => op(typed.Expr.ToFloat(typedLeft), typedRight, typed.Type.Float)
    case (typed.Type.Float, typed.Type.Int) => op(typedLeft, typed.Expr.ToFloat(typedRight), typed.Type.Float)
    case (typed.Type.Float, typed.Type.Float) => op(typed.Expr.ToFloat(typedLeft), typed.Expr.ToFloat(typedRight), typed.Type.Float)
    case (leftType, rightType) =>
      write(TypeError.Mismatch(
        expected = List(
          TypePattern.Operator(
            TypePattern.Union(List(TypePattern.Type(typed.Type.Int), TypePattern.Type(typed.Type.Float))),
            TypePattern.Union(List(TypePattern.Type(typed.Type.Int), TypePattern.Type(typed.Type.Float))),
            opText
          )
        ),
        got = List(leftType, rightType)
      ))
      op(typedLeft, typedRight, typed.Type.Invalid)

def typeStatement(statement: resolved.Statement): Typing[typed.Statement] = statement match
  case definition: resolved.Definition => ???
  case expr: resolved.Expr => typeExpr(expr)

def typeDefinition(definition: resolved.Definition): Typing[typed.Definition] = definition match
  case resolved.Definition.Val(symbol, tpe, expr, mutable, span) =>
    val resolvedType = resolveType(tpe)
    TypeContext.assignType(symbol, resolvedType)
    typed.Definition.Val(symbol, resolvedType, typeExprTo(expr, resolvedType), mutable, span)
  case resolved.Definition.Function(symbol, params, retType, body, span) =>
    val resolvedParams = params.map((name, tpe) => (name, resolveType(tpe)))
    val resolvedRetType = resolveType(retType)
    TypeContext.assignType(symbol, typed.Type.Function(resolvedParams.map(_._2), resolvedRetType))
    typed.Definition.Function(symbol, resolvedParams, resolvedRetType, typeExprTo(body, resolvedRetType), span)

def typeExpr(expr: resolved.Expr): Typing[typed.Expr] = expr match
  case resolved.Expr.LBool(value, span)                  => typed.Expr.LBool(value, typed.Type.Boolean, span)
  case resolved.Expr.LInt(value, span)                   => typed.Expr.LInt(value, typed.Type.Int, span)
  case resolved.Expr.LFloat(value, span)                 => typed.Expr.LFloat(value, typed.Type.Float, span)
  case resolved.Expr.LChar(value, span)                  => typed.Expr.LChar(value, typed.Type.Char, span)
  case resolved.Expr.LString(value, span)                => typed.Expr.LString(value, typed.Type.String, span)
  case resolved.Expr.Not(expr, span)                     => typed.Expr.Not(typeExprTo(expr, typed.Type.Boolean), typed.Type.Boolean, span)
  case resolved.Expr.Equal(left, right, span)            => typed.Expr.Equal(typeExpr(left), typeExpr(right), typed.Type.Boolean, span)
  case resolved.Expr.NotEqual(left, right, span)         => typed.Expr.NotEqual(typeExpr(left), typeExpr(right), typed.Type.Boolean, span)
  case resolved.Expr.Less(left, right, span)             => typed.Expr.Less(assertNumeric(typeExpr(expr)), assertNumeric(typeExpr(expr)), typed.Type.Boolean, span)
  case resolved.Expr.LessEqual(left, right, span)        => typed.Expr.LessEqual(assertNumeric(typeExpr(expr)), assertNumeric(typeExpr(expr)), typed.Type.Boolean, span)
  case resolved.Expr.Greater(left, right, span)          => typed.Expr.Greater(assertNumeric(typeExpr(expr)), assertNumeric(typeExpr(expr)), typed.Type.Boolean, span)
  case resolved.Expr.GreaterEqual(left, right, span)     => typed.Expr.GreaterEqual(assertNumeric(typeExpr(expr)), assertNumeric(typeExpr(expr)), typed.Type.Boolean, span)
  case resolved.Expr.Plus(expr, span)                    =>
    val typedExpr = typeExpr(expr)
    typed.Expr.Plus(assertNumeric(typedExpr), typedExpr.tpe, span)
  case resolved.Expr.Minus(expr, span)                   =>
    val typedExpr = typeExpr(expr)
    typed.Expr.Minus(assertNumeric(typedExpr), typedExpr.tpe, span)
  case resolved.Expr.Add(left, right, span)              => typeBinaryNumOp(left, right, "+", typed.Expr.Add(_, _, _, span))
  case resolved.Expr.Sub(left, right, span)              => typeBinaryNumOp(left, right, "-", typed.Expr.Sub(_, _, _, span))
  case resolved.Expr.Mul(left, right, span)              => typeBinaryNumOp(left, right, "*", typed.Expr.Mul(_, _, _, span))
  case resolved.Expr.Div(left, right, span)              => typeBinaryNumOp(left, right, "/", typed.Expr.Div(_, _, _, span))
  case resolved.Expr.IntDiv(left, right, span)           => typeBinaryNumOp(left, right, "//", typed.Expr.IntDiv(_, _, _, span))
  case resolved.Expr.Mod(left, right, span)              => typeBinaryNumOp(left, right, "+", typed.Expr.Mod(_, _, _, span))
  case resolved.Expr.And(left, right, span)              => typed.Expr.And(typeExprTo(left, typed.Type.Boolean), typeExprTo(right, typed.Type.Boolean), typed.Type.Boolean, span)
  case resolved.Expr.Or(left, right, span)               => typed.Expr.Or(typeExprTo(left, typed.Type.Boolean), typeExprTo(right, typed.Type.Boolean), typed.Type.Boolean, span)
  case resolved.Expr.VarCall(symbol, span)               => typed.Expr.VarCall(symbol, TypeContext.getType(symbol), span)
  case resolved.Expr.Assign(symbol, expr, span)          => typed.Expr.Assign(symbol, typeExprTo(expr, TypeContext.getType(symbol)), typed.Type.Unit, span)
  case resolved.Expr.Apply(expr, args, span)             =>
    val typedExpr = typeExpr(expr)
    val typedArgs = args.map(typeExpr)
    typedExpr.tpe match
      case typed.Type.Function(inputs, output) =>
        if inputs.sizeCompare(args) == 0 then write(TypeError.ApplyMismatch(inputs, typedArgs.map(_.tpe)))
        typed.Expr.Apply(typedExpr, typedArgs.zip(inputs).map(castExpr), output, span)
      case _ =>
        write(TypeError.ApplyOnNonFunction(typedExpr.tpe))
        typed.Expr.Apply(typedExpr, typedArgs, typed.Type.Invalid, span)
    
  case resolved.Expr.Block(statements, span)             =>
    val typedStatements = statements.map(typeStatement)
    val blockType = typedStatements.lastOption match
      case Some(expr: typed.Expr) => expr.tpe
      case _ => typed.Type.Unit
    
    typed.Expr.Block(typedStatements, blockType, span)
  case resolved.Expr.If(cond, ifTrue, ifFalse, span)     =>
    val typedCond = typeExprTo(cond, typed.Type.Boolean)
    val typedIfTrue = typeExpr(ifTrue)
    val typedIfFalse = typeExpr(ifFalse)
    val ifType = union(typedIfTrue.tpe, typedIfFalse.tpe)

    typed.Expr.If(typedCond, castExpr(typedIfTrue, ifType), castExpr(typedIfFalse, ifType), ifType, span)

  case resolved.Expr.While(cond, body, span)             => typed.Expr.While(typeExprTo(cond, typed.Type.Boolean), typeExpr(body), typed.Type.Unit, span)
  case resolved.Expr.For(iterator, iterable, body, span) =>
    TypeContext.assignType(iterator, typed.Type.Any)
    typed.Expr.For(iterator, typeExpr(iterable), typeExpr(body), typed.Type.Unit, span)
  case resolved.Expr.Invalid(span)                       => typed.Expr.Invalid(typed.Type.Invalid, span)