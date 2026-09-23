package org.algorab.resolution

import io.github.iltotore.iron.autoRefine
import io.github.iltotore.pureparser.Span
import org.algorab.AlgorabProgram
import org.algorab.ast.Identifier
import org.algorab.ast.ScopeId
import org.algorab.ast.Symbol
import org.algorab.ast.Symbol.Namespace
import org.algorab.ast.SymbolId
import org.algorab.ast.raw
import org.algorab.ast.resolved
import purelogic.*
import scala.annotation.tailrec
import org.algorab.ast.raw.Import.Selector

/**
 * The name resolution phase.
 */
object Resolver:

  /**
   * Declare a package if needed and resolve it.
   *
   * @param ownerId the root owner of the packages to create
   * @param scopes the current scope path
   * @param path the remaining package path to declare/resolve
   * @return the id of the package and its scope path, typically for `package a.b.c` it will be `List(scopeA, scopeB, scopeC)`
   */
  @tailrec
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

  def declareProgramPackage(program: raw.Program): Resolution[(SymbolId, List[ScopeId])] =
    declarePackage(SymbolId.Root, List(ScopeId.Root), program.packageName)

  /**
   * Declare all qualified members of a parsed file.
   *
   * @param program the parsed file to visit
   * @return the package id and scope path of the root of this source file
   */
  def declareProgram(program: raw.Program, owner: SymbolId, packageScope: List[ScopeId]): Resolution[Unit] =
    ResolutionContext.inScopePath(packageScope)(
      declareAllStatements(program.statements, owner == SymbolId.Root)
    )

  /**
   * Resolve the given parsed file.
   *
   * @param program the parsed file to resolve
   * @param owner the owner of this parsed file aka its package, possibly [[SymbolId.Root]] for package-less files
   * @param packageScope the scope path of the root of this source file
   * @return a representation of the same file with all its names resolved
   */
  def resolveProgram(program: raw.Program, owner: SymbolId, packageScope: List[ScopeId]): Resolution[resolved.Program] =
    if owner == SymbolId.Root then
      resolved.Program.Script(
        statements = ResolutionContext.inScopePath(packageScope)(program.statements.flatMap(resolveStatement))
      )
    else
      resolved.Program.Module(
        owner = owner,
        definitions = ResolutionContext.inScopePath(packageScope)(
          program
            .statements
            .flatMap:
              case definition: raw.Definition => Some(resolveDefinition(definition))
              case importClause: raw.Import =>
                resolveImport(importClause)
                None
              case expr: raw.Expr =>
                write(ResolutionError.TopLevelStatementInModule(expr.span))
                None
        )
      )

  /**
   * Resolve the given type.
   *
   * @param tpe the type to resolve
   * @param span the span where the resolution occurs, used for reporting purpose
   * @return a representation of the same type with all its names resolved
   */
  def resolveType(tpe: raw.Type, span: Span): Resolution[resolved.Type] = tpe match
    case raw.Type.Ref(name) => resolved.Type.Ref(ResolutionContext.getLocalType(name, span))
    case raw.Type.Inferred  => resolved.Type.Inferred

  /**
   * Declare all given statements.
   *
   * @param statements the statements to declare
   * @param isBlock whether these statements reside in a [[raw.Expr.Block]] or not
   */
  def declareAllStatements(statements: List[raw.Statement], isBlock: Boolean): Resolution[Unit] =
    statements.foreach:
      case definition: raw.Definition => declareDefinition(definition, isBlock)
      case _                          =>

  /**
   * Resolve the given statement.
   *
   * @param statement the statement to resolve
   * @return a representation of the same statement with all its names resolved
   */
  def resolveStatement(statement: raw.Statement): Resolution[Option[resolved.Statement]] = statement match
    case importClause: raw.Import   =>
      resolveImport(importClause)
      None
    case definition: raw.Definition => Some(resolveDefinition(definition))
    case expr: raw.Expr             => Some(resolveExpr(expr))

  def resolveImport(importClause: raw.Import): Resolution[Unit] =
    val (head, headSpan) :: tail = importClause.path.runtimeChecked
    val (qualifier, qualifierSpan) = tail.foldLeft((ResolutionContext.getLocalTerm(head, headSpan), headSpan)):
        case ((symbol, symbolSpan), (segment, segmentSpan)) =>
          if symbol == SymbolId.Invalid then (symbol, symbolSpan)
          else (ResolutionContext.getMemberTermOrFail(symbol, segment, symbolSpan, segmentSpan), segmentSpan)

    resolveSelector(qualifier, importClause.selector, qualifierSpan)

  /**
   * Declare the given definition.
   *
   * @param definition the definition to declare
   * @param isBlock whether this definition reside in a [[raw.Expr.Block]] or not
   */
  def declareDefinition(definition: raw.Definition, isBlock: Boolean): Resolution[Unit] = definition match
    case raw.Definition.Val(name, _, expr, mutable, span) =>
      ResolutionContext.declareTerm(Symbol.Variable(_, name, None, mutable, span), initialized = !isBlock).asInstanceOf[Unit]
    case raw.Definition.Function(name, _, _, body, span) =>
      ResolutionContext.declareTerm(Symbol.Function(_, name, None, span)).asInstanceOf[Unit]

  /**
   * Resolve the given definition.
   *
   * @param statement the definition to resolve
   * @return a representation of the same definition with all its names resolved
   */
  def resolveDefinition(definition: raw.Definition): Resolution[resolved.Definition] = definition match
    case raw.Definition.Val(name, tpe, expr, mutable, span) =>
      ResolutionContext.initializeLocalTerm(name)
      ResolutionContext.assignDeclaration(ResolutionContext.getLocalTerm(name, span))(
        resolved.Definition.Val(
          ResolutionContext.getLocalTerm(name, span),
          resolveType(tpe, span),
          resolveExpr(expr),
          mutable,
          span
        )
      )
    case raw.Definition.Function(name, params, retType, body, span) =>
      val id = ResolutionContext.getLocalTerm(name, span)
      ResolutionContext.inNewScope(ResolutionContext.getOwner(id))(
        ResolutionContext.assignDeclaration(id)(
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
      )

  /**
   * Resolve the given expression.
   *
   * @param expr the expression to resolve
   * @return a representation of the same expression with all its names resolved
   */
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
    case raw.Expr.Select(expr, member, span) =>
      resolveExpr(expr) match
        case resolved.Expr.VarCall(symbol, symbolSpan) =>
          resolved.Expr.VarCall(
            ResolutionContext.getMemberTermOrFail(symbol, member, symbolSpan, span),
            span
          )

        case resolvedExpr => resolved.Expr.Select(resolvedExpr, member, span)

    case raw.Expr.Apply(expr, args, span) => resolved.Expr.Apply(resolveExpr(expr), args.map(resolveExpr), span)
    case raw.Expr.Block(statements, span) => ResolutionContext.inNewScope(None):
        declareAllStatements(statements, true)
        resolved.Expr.Block(statements.flatMap(resolveStatement), span)
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

  def resolveSelector(qualifier: SymbolId, selector: Selector, qualifierSpan: Span): Resolution[Unit] = selector match
    case Selector.Simple(name, span) =>
      ResolutionContext.importMember(qualifier, name, name, qualifierSpan, span)

    case Selector.Wildcard(span) =>
      get.symbols(qualifier) match
        case namespace: Namespace =>
          val namespaceScope = get.scopes(namespace.memberScope)
          ResolutionContext.updateCurrentScope(scope => scope.copy(
            localTerms = scope.localTerms ++ namespaceScope.localTerms,
            localTypes = scope.localTypes ++ namespaceScope.localTypes
          ))
        case sym =>
          write(ResolutionError.NotANamespace(sym, qualifierSpan))

    case Selector.Rename(name, alias, span) =>
      ResolutionContext.importMember(qualifier, name, alias, qualifierSpan, span)
      

  /**
   * Resolve the names of the given programs.
   *
   * @param programs the parsed programs to resolve
   * @return the resolved programs and the resolution context, containing information about the declared modules and symbols
   */
  def apply(programs: Seq[raw.Program]): AlgorabProgram[(ResolutionContext, Seq[resolved.Program])] =
    Resolution:
      val declaredPackages = programs.map(program => (program, Resolver.declareProgramPackage(program)))
      if declaredPackages.count(_._2._1 == SymbolId.Root) > 1 then
        write(ResolutionError.MultipleScriptFiles(Span(0, 0)))
        fail(())
      else
        declaredPackages
        .tapEach:
          case (program, (packageId, packageScope)) => Resolver.declareProgram(program, packageId, packageScope)
        .map:
          case (program, (packageId, packageScope)) => Resolver.resolveProgram(program, packageId, packageScope)
