package org.algorab.resolution

import org.algorab.ast.raw
import org.algorab.ast.resolved
import org.algorab.ast.Symbol
import io.github.iltotore.pureparser.Span

object Resolver:

  def resolveType(tpe: raw.Type, span: Span): Resolution[resolved.Type] = tpe match
    case raw.Type.Ref(name) => resolved.Type.Ref(ResolutionContext.getLocalType(name, span))
    case raw.Type.Inferred  => resolved.Type.Inferred

  def resolveStatement(statement: raw.Statement): Resolution[resolved.Statement] = statement match
    case definition: raw.Definition => resolveDefinition(definition)
    case expr: raw.Expr             => resolveExpr(expr)

  def resolveDefinition(definition: raw.Definition): Resolution[resolved.Definition] = definition match
    case raw.Definition.Val(name, tpe, expr, mutable, span) =>
      ResolutionContext.markInitialized(name)
      resolved.Definition.Val(ResolutionContext.getLocalTerm(name, span), resolveType(tpe, span), resolveExpr(expr), mutable, span)
    case raw.Definition.Function(name, params, retType, body, span) =>
      val id = ResolutionContext.getLocalTerm(name, span)
      ResolutionContext.inNewScope(ResolutionContext.getOwner(id))(
        resolved.Definition.Function(
          id,
          params.map((name, tpe) => (
            ResolutionContext.declareTerm(Symbol.Variable(_, name, None, false, span)),
            resolveType(tpe, span)
          )),
          resolveType(retType, span),
          resolveExpr(body),
          span
        )
      )

  def addDefinition(definition: raw.Definition): Resolution[Unit] = definition match
    case raw.Definition.Val(name, _, _, mutable, span)      =>
      ResolutionContext.declareTerm(Symbol.Variable(_, name, None, mutable, span), initialized = false).asInstanceOf[Unit]
    case raw.Definition.Function(name, _, _, _, span) =>
      ResolutionContext.declareTerm(Symbol.Function(_, name, None, span)).asInstanceOf[Unit]

  def resolveExpr(expr: raw.Expr): Resolution[resolved.Expr] = expr match
    case raw.Expr.LBool(value, span)              => resolved.Expr.LBool(value, span)
    case raw.Expr.LInt(value, span)               => resolved.Expr.LInt(value, span)
    case raw.Expr.LFloat(value, span)             => resolved.Expr.LFloat(value, span)
    case raw.Expr.LChar(value, span)              => resolved.Expr.LChar(value, span)
    case raw.Expr.LString(value, span)            => resolved.Expr.LString(value, span)
    case raw.Expr.Not(expr, span)                 => resolved.Expr.Not(resolveExpr(expr), span)
    case raw.Expr.Equal(left, right, span)        => resolved.Expr.Equal(resolveExpr(left), resolveExpr(right), span)
    case raw.Expr.NotEqual(left, right, span)     => resolved.Expr.NotEqual(resolveExpr(left), resolveExpr(right), span)
    case raw.Expr.Less(left, right, span)         => resolved.Expr.Less(resolveExpr(left), resolveExpr(right), span)
    case raw.Expr.LessEqual(left, right, span)    => resolved.Expr.LessEqual(resolveExpr(left), resolveExpr(right), span)
    case raw.Expr.Greater(left, right, span)      => resolved.Expr.Greater(resolveExpr(left), resolveExpr(right), span)
    case raw.Expr.GreaterEqual(left, right, span) => resolved.Expr.GreaterEqual(resolveExpr(left), resolveExpr(right), span)
    case raw.Expr.Plus(expr, span)                => resolved.Expr.Plus(resolveExpr(expr), span)
    case raw.Expr.Minus(expr, span)               => resolved.Expr.Minus(resolveExpr(expr), span)
    case raw.Expr.Add(left, right, span)          => resolved.Expr.Add(resolveExpr(left), resolveExpr(right), span)
    case raw.Expr.Sub(left, right, span)          => resolved.Expr.Sub(resolveExpr(left), resolveExpr(right), span)
    case raw.Expr.Mul(left, right, span)          => resolved.Expr.Mul(resolveExpr(left), resolveExpr(right), span)
    case raw.Expr.Div(left, right, span)          => resolved.Expr.Div(resolveExpr(left), resolveExpr(right), span)
    case raw.Expr.IntDiv(left, right, span)       => resolved.Expr.IntDiv(resolveExpr(left), resolveExpr(right), span)
    case raw.Expr.Mod(left, right, span)          => resolved.Expr.Mod(resolveExpr(left), resolveExpr(right), span)
    case raw.Expr.And(left, right, span)          => resolved.Expr.And(resolveExpr(left), resolveExpr(right), span)
    case raw.Expr.Or(left, right, span)           => resolved.Expr.Or(resolveExpr(left), resolveExpr(right), span)
    case raw.Expr.VarCall(name, span)             => resolved.Expr.VarCall(ResolutionContext.getLocalTerm(name, span), span)
    case raw.Expr.Assign(name, expr, span)        => resolved.Expr.Assign(ResolutionContext.getLocalTerm(name, span), resolveExpr(expr), span)
    case raw.Expr.Apply(expr, args, span)         => resolved.Expr.Apply(resolveExpr(expr), args.map(resolveExpr), span)
    case raw.Expr.Block(statements, span) =>
      ResolutionContext.inNewScope(None):
        statements.foreach:
          case definition: raw.Definition => addDefinition(definition)
          case _                      =>
        resolved.Expr.Block(statements.map(resolveStatement), span)
    case raw.Expr.If(cond, ifTrue, ifFalse, span) => resolved.Expr.If(resolveExpr(cond), resolveExpr(ifTrue), resolveExpr(ifFalse), span)
    case raw.Expr.While(cond, body, span)         => resolved.Expr.While(resolveExpr(cond), resolveExpr(body), span)
    case raw.Expr.For(iterator, iterable, body, span) =>
      ResolutionContext.inNewScope(None)(
        resolved.Expr.For(
          ResolutionContext.declareTerm(Symbol.Variable(_, iterator, None, false, span)),
          resolveExpr(iterable),
          resolveExpr(body),
          span
        )
      )
    case raw.Expr.Invalid(span) => resolved.Expr.Invalid(span)
