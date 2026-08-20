package org.algorab.resolution

import io.github.iltotore.iron.autoRefine
import io.github.iltotore.pureparser.Span
import org.algorab.ast.Identifier
import org.algorab.ast.ScopeId
import org.algorab.ast.Symbol
import org.algorab.ast.Symbol.Namespace
import org.algorab.ast.SymbolId
import org.algorab.ast.raw
import org.algorab.ast.resolved
import purelogic.*
import scala.annotation.tailrec
import org.algorab.resolution.ResolutionContext.currentScope

object Resolver:

  def declarePackage(ownerId: SymbolId, scopes: List[ScopeId], path: List[(Identifier, Span)]): Resolution[(SymbolId, List[ScopeId])] = path match
    case Nil => (ownerId, scopes)
    case (head, headSpan) :: tail =>
      val headScope = get.scopes(scopes.head)
      headScope.localTerms.get(head) match
        case Some((id, _)) => get.symbols(id) match
            case namespace: Symbol.Namespace => declarePackage(id, namespace.memberScope :: scopes, tail)
            case other =>
              write(ResolutionError.NotANamespace(other, headSpan))
              (SymbolId.Invalid, List(ScopeId.Invalid))
        case None =>
          val packageId = get.nextSymbolId
          val scopeId = get.nextScopeId
          val packageSymbol = ResolutionContext.declareSymbol(Symbol.Package(
            packageId,
            head,
            Some(ownerId),
            scopeId
          ))

          update(context =>
            context.copy(
              scopes = context.scopes
                .updated(context.nextScopeId, ResolutionScope.empty(Some(packageId)))
                .updated(scopes.head, headScope.withLocalTerm(head, packageId, true)),
              nextScopeId = context.nextScopeId + 1
            )
          )

          declarePackage(packageId, scopeId :: scopes, tail)

  def declareProgram(program: raw.Program): Resolution[(SymbolId, List[ScopeId])] =
    val (packageId, packageScope) = declarePackage(SymbolId.Root, List(ScopeId.Root), program.packageName)
    ResolutionContext.inScopePath(packageScope)(declareAllStatements(program.statements, packageId == SymbolId.Root))
    (packageId, packageScope)

  def resolveProgram(program: raw.Program, owner: SymbolId, packageScope: List[ScopeId]): Resolution[resolved.Program] =
    resolved.Program(
      owner = owner,
      statements = ResolutionContext.inScopePath(packageScope)(program.statements.map(resolveStatement))
    )

  def resolveType(tpe: raw.Type, span: Span): Resolution[resolved.Type] = tpe match
    case raw.Type.Ref(name) => resolved.Type.Ref(ResolutionContext.getLocalType(name, span))
    case raw.Type.Inferred  => resolved.Type.Inferred

  def declareAllStatements(statements: List[raw.Statement], isBlock: Boolean): Resolution[Unit] =
    statements.foreach:
      case definition: raw.Definition => declareDefinition(definition, isBlock)
      case _                          =>

  def resolveStatement(statement: raw.Statement): Resolution[resolved.Statement] = statement match
    case definition: raw.Definition => resolveDefinition(definition)
    case expr: raw.Expr             => resolveExpr(expr)

  def resolveDefinition(definition: raw.Definition): Resolution[resolved.Definition] = definition match
    case raw.Definition.Val(name, tpe, expr, mutable, span) =>
      ResolutionContext.initializeLocalTerm(name)
      resolved.Definition.Val(
        ResolutionContext.getLocalTerm(name, span),
        resolveType(tpe, span),
        resolveExpr(expr),
        mutable,
        span
      )
    case raw.Definition.Function(name, params, retType, body, span) =>
      val id = ResolutionContext.getLocalTerm(name, span)
      ResolutionContext.inNewScope(ResolutionContext.getOwner(id))(
        resolved.Definition.Function(
          id,
          params.map((name, tpe) =>
            (
              ResolutionContext.declareTerm(Symbol.Variable(_, name, None, false, span)),
              resolveType(tpe, span)
            )
          ),
          resolveType(retType, span),
          resolveExpr(body),
          span
        )
      )

  def declareDefinition(definition: raw.Definition, isBlock: Boolean): Resolution[Unit] = definition match
    case raw.Definition.Val(name, _, _, mutable, span) =>
      ResolutionContext.declareTerm(Symbol.Variable(_, name, None, mutable, span), initialized = !isBlock).asInstanceOf[Unit]
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
    case raw.Expr.Block(statements, span) => ResolutionContext.inNewScope(None):
        declareAllStatements(statements, true)
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
