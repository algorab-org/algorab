package org.algorab.resolution

import org.algorab.ast.Definition
import org.algorab.ast.Expr
import org.algorab.ast.Statement
import org.algorab.ast.Type

object Resolver:

  def resolveType(tpe: Type): Resolution[Type] = tpe match
    case Type.Ref(name) => Type.Ref(name)
    case Type.Inferred  => Type.Inferred

  def resolveStatement(statement: Statement): Resolution[Statement] = statement match
    case definition: Definition => resolveDefinition(definition)
    case expr: Expr             => resolveExpr(expr)

  def resolveDefinition(definition: Definition): Resolution[Definition] = definition match
    case Definition.Val(name, tpe, expr, mutable, span) =>
      ResolutionContext.updatedResolvedDef(name)(_.copy(initialized = true))
      Definition.Val(ResolutionContext.getResolvedName(name), resolveType(tpe), resolveExpr(expr), mutable, span)
    case Definition.Function(name, params, retType, body, span) =>
      val funName = ResolutionContext.getResolvedName(name)
      ResolutionContext.inNewScope(funName):
        Definition.Function(
          funName,
          params.map((name, tpe) => (ResolutionContext.addName(name), tpe)),
          resolveType(retType),
          resolveExpr(body),
          span
        )

  def addDefinition(definition: Definition): Resolution[Unit] = definition match
    case Definition.Val(name, _, _, _, _)      => ResolutionContext.addName(name, initialized = false).asInstanceOf[Unit]
    case Definition.Function(name, _, _, _, _) => ResolutionContext.addName(name).asInstanceOf[Unit]

  def resolveExpr(expr: Expr): Resolution[Expr] = expr match
    case Expr.LBool(value, span)              => Expr.LBool(value, span)
    case Expr.LInt(value, span)               => Expr.LInt(value, span)
    case Expr.LFloat(value, span)             => Expr.LFloat(value, span)
    case Expr.LChar(value, span)              => Expr.LChar(value, span)
    case Expr.LString(value, span)            => Expr.LString(value, span)
    case Expr.Not(expr, span)                 => Expr.Not(resolveExpr(expr), span)
    case Expr.Equal(left, right, span)        => Expr.Equal(resolveExpr(left), resolveExpr(right), span)
    case Expr.NotEqual(left, right, span)     => Expr.NotEqual(resolveExpr(left), resolveExpr(right), span)
    case Expr.Less(left, right, span)         => Expr.Less(resolveExpr(left), resolveExpr(right), span)
    case Expr.LessEqual(left, right, span)    => Expr.LessEqual(resolveExpr(left), resolveExpr(right), span)
    case Expr.Greater(left, right, span)      => Expr.Greater(resolveExpr(left), resolveExpr(right), span)
    case Expr.GreaterEqual(left, right, span) => Expr.GreaterEqual(resolveExpr(left), resolveExpr(right), span)
    case Expr.Plus(expr, span)                => Expr.Plus(resolveExpr(expr), span)
    case Expr.Minus(expr, span)               => Expr.Minus(resolveExpr(expr), span)
    case Expr.Add(left, right, span)          => Expr.Add(resolveExpr(left), resolveExpr(right), span)
    case Expr.Sub(left, right, span)          => Expr.Sub(resolveExpr(left), resolveExpr(right), span)
    case Expr.Mul(left, right, span)          => Expr.Mul(resolveExpr(left), resolveExpr(right), span)
    case Expr.Div(left, right, span)          => Expr.Div(resolveExpr(left), resolveExpr(right), span)
    case Expr.IntDiv(left, right, span)       => Expr.IntDiv(resolveExpr(left), resolveExpr(right), span)
    case Expr.Mod(left, right, span)          => Expr.Mod(resolveExpr(left), resolveExpr(right), span)
    case Expr.And(left, right, span)          => Expr.And(resolveExpr(left), resolveExpr(right), span)
    case Expr.Or(left, right, span)           => Expr.Or(resolveExpr(left), resolveExpr(right), span)
    case Expr.VarCall(name, span)             => Expr.VarCall(ResolutionContext.getResolvedName(name), span)
    case Expr.Assign(name, expr, span)        => Expr.Assign(ResolutionContext.getResolvedName(name), resolveExpr(expr), span)
    case Expr.Apply(expr, args, span)         => Expr.Apply(resolveExpr(expr), args.map(resolveExpr), span)
    case Expr.Block(statements, span) =>
      ResolutionContext.inNewAnonymScope:
        statements.foreach:
          case definition: Definition => addDefinition(definition)
          case _                      =>
        Expr.Block(statements.map(resolveStatement), span)
    case Expr.If(cond, ifTrue, ifFalse, span) => Expr.If(resolveExpr(cond), resolveExpr(ifTrue), resolveExpr(ifFalse), span)
    case Expr.While(cond, body, span)         => Expr.While(resolveExpr(cond), resolveExpr(body), span)
    case Expr.For(iterator, iterable, body, span) =>
      ResolutionContext.inNewAnonymScope:
        Expr.For(ResolutionContext.addName(iterator), resolveExpr(iterable), resolveExpr(body), span)
    case Expr.Invalid(span) => Expr.Invalid(span)
