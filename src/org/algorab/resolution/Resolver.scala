package org.algorab.resolution

import io.github.iltotore.iron.autoRefine
import org.algorab.AlgorabProgram
import org.algorab.ast.Identifier
import org.algorab.ast.ScopeId
import org.algorab.ast.Symbol
import org.algorab.ast.Symbol.Namespace
import org.algorab.ast.SymbolId
import org.algorab.ast.raw
import org.algorab.ast.raw.Import.Selector
import org.algorab.ast.resolved
import org.algorab.util.SourcePosition
import purelogic.*
import scala.annotation.tailrec

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
  def declarePackage(ownerId: SymbolId, scopes: List[ScopeId], path: List[(Identifier, SourcePosition)]): Resolution[(SymbolId, List[ScopeId])] =
    path match
      case Nil => (ownerId, scopes)
      case (head, headPosition) :: tail =>
        val headScope = get.scopes(scopes.head)
        headScope.localTerms.get(head) match
          case Some((id, _)) => get.symbols(id) match
              case namespace: Symbol.Namespace => declarePackage(id, namespace.memberScope :: scopes, tail)
              case other =>
                write(ResolutionError.NotANamespace(other, headPosition))
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
              case _: raw.Expr.Invalid => None
              case expr: raw.Expr =>
                write(ResolutionError.TopLevelStatementInModule(expr.position))
                None
        )
      )

  /**
   * Resolve the given type.
   *
   * @param tpe the type to resolve
   * @param position the position where the resolution occurs, used for reporting purpose
   * @return a representation of the same type with all its names resolved
   */
  def resolveType(tpe: raw.Type, position: SourcePosition): Resolution[resolved.Type] = tpe match
    case raw.Type.Ref(name) => resolved.Type.Ref(ResolutionContext.getLocalType(name, position))
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
    case importClause: raw.Import =>
      resolveImport(importClause)
      None
    case definition: raw.Definition => Some(resolveDefinition(definition))
    case expr: raw.Expr             => Some(resolveExpr(expr))

  /**
   * Resolve the given import clause.
   *
   * @param importClause the import clause to resolve
   */
  def resolveImport(importClause: raw.Import): Resolution[Unit] =
    val (head, headPosition) :: tail = importClause.path.runtimeChecked
    val (qualifier, qualifierPosition) = tail.foldLeft((ResolutionContext.getLocalTerm(head, headPosition), headPosition)):
      case ((symbol, symbolPosition), (segment, segmentPosition)) =>
        if symbol == SymbolId.Invalid then (symbol, symbolPosition)
        else (ResolutionContext.getMemberTermOrFail(symbol, segment, symbolPosition, segmentPosition), segmentPosition)

    resolveSelector(qualifier, importClause.selector, qualifierPosition)

  /**
   * Declare the given definition.
   *
   * @param definition the definition to declare
   * @param isBlock whether this definition reside in a [[raw.Expr.Block]] or not
   */
  def declareDefinition(definition: raw.Definition, isBlock: Boolean): Resolution[Unit] = definition match
    case raw.Definition.Val(name, _, expr, mutable, position) =>
      ResolutionContext.declareTerm(Symbol.Variable(_, name, None, mutable, position), initialized = !isBlock).asInstanceOf[Unit]
    case raw.Definition.Function(name, _, _, body, position) =>
      ResolutionContext.declareTerm(Symbol.Function(_, name, None, position)).asInstanceOf[Unit]

  /**
   * Resolve the given definition.
   *
   * @param statement the definition to resolve
   * @return a representation of the same definition with all its names resolved
   */
  def resolveDefinition(definition: raw.Definition): Resolution[resolved.Definition] = definition match
    case raw.Definition.Val(name, tpe, expr, mutable, position) =>
      ResolutionContext.initializeLocalTerm(name)
      ResolutionContext.assignDeclaration(ResolutionContext.getLocalTerm(name, position))(
        resolved.Definition.Val(
          ResolutionContext.getLocalTerm(name, position),
          resolveType(tpe, position),
          resolveExpr(expr),
          mutable,
          position
        )
      )
    case raw.Definition.Function(name, params, retType, body, position) =>
      val id = ResolutionContext.getLocalTerm(name, position)
      ResolutionContext.inNewScope(ResolutionContext.getOwner(id))(
        ResolutionContext.assignDeclaration(id)(
          resolved.Definition.Function(
            id,
            params.map((name, tpe) =>
              (
                ResolutionContext.declareTerm(Symbol.Variable(_, name, None, false, position)),
                resolveType(tpe, position)
              )
            ),
            resolveType(retType, position),
            resolveExpr(body),
            position
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
    case raw.Expr.LBool(value, position)              => resolved.Expr.LBool(value, position)
    case raw.Expr.LInt(value, position)               => resolved.Expr.LInt(value, position)
    case raw.Expr.LFloat(value, position)             => resolved.Expr.LFloat(value, position)
    case raw.Expr.LChar(value, position)              => resolved.Expr.LChar(value, position)
    case raw.Expr.LString(value, position)            => resolved.Expr.LString(value, position)
    case raw.Expr.Not(expr, position)                 => resolved.Expr.Not(resolveExpr(expr), position)
    case raw.Expr.Equal(left, right, position)        => resolved.Expr.Equal(resolveExpr(left), resolveExpr(right), position)
    case raw.Expr.NotEqual(left, right, position)     => resolved.Expr.NotEqual(resolveExpr(left), resolveExpr(right), position)
    case raw.Expr.Less(left, right, position)         => resolved.Expr.Less(resolveExpr(left), resolveExpr(right), position)
    case raw.Expr.LessEqual(left, right, position)    => resolved.Expr.LessEqual(resolveExpr(left), resolveExpr(right), position)
    case raw.Expr.Greater(left, right, position)      => resolved.Expr.Greater(resolveExpr(left), resolveExpr(right), position)
    case raw.Expr.GreaterEqual(left, right, position) => resolved.Expr.GreaterEqual(resolveExpr(left), resolveExpr(right), position)
    case raw.Expr.Plus(expr, position)                => resolved.Expr.Plus(resolveExpr(expr), position)
    case raw.Expr.Minus(expr, position)               => resolved.Expr.Minus(resolveExpr(expr), position)
    case raw.Expr.Add(left, right, position)          => resolved.Expr.Add(resolveExpr(left), resolveExpr(right), position)
    case raw.Expr.Sub(left, right, position)          => resolved.Expr.Sub(resolveExpr(left), resolveExpr(right), position)
    case raw.Expr.Mul(left, right, position)          => resolved.Expr.Mul(resolveExpr(left), resolveExpr(right), position)
    case raw.Expr.Div(left, right, position)          => resolved.Expr.Div(resolveExpr(left), resolveExpr(right), position)
    case raw.Expr.IntDiv(left, right, position)       => resolved.Expr.IntDiv(resolveExpr(left), resolveExpr(right), position)
    case raw.Expr.Mod(left, right, position)          => resolved.Expr.Mod(resolveExpr(left), resolveExpr(right), position)
    case raw.Expr.And(left, right, position)          => resolved.Expr.And(resolveExpr(left), resolveExpr(right), position)
    case raw.Expr.Or(left, right, position)           => resolved.Expr.Or(resolveExpr(left), resolveExpr(right), position)
    case raw.Expr.VarCall(name, position)             => resolved.Expr.VarCall(ResolutionContext.getLocalTerm(name, position), position)
    case raw.Expr.Assign(name, expr, position) => resolved.Expr.Assign(ResolutionContext.getLocalTerm(name, position), resolveExpr(expr), position)
    case raw.Expr.Select(expr, member, position) =>
      resolveExpr(expr) match
        case resolved.Expr.VarCall(symbol, symbolPosition) =>
          resolved.Expr.VarCall(
            ResolutionContext.getMemberTermOrFail(symbol, member, symbolPosition, position),
            position
          )

        case resolvedExpr => resolved.Expr.Select(resolvedExpr, member, position)

    case raw.Expr.Apply(expr, args, position) => resolved.Expr.Apply(resolveExpr(expr), args.map(resolveExpr), position)
    case raw.Expr.Block(statements, position) => ResolutionContext.inNewScope(None):
        declareAllStatements(statements, true)
        resolved.Expr.Block(statements.flatMap(resolveStatement), position)
    case raw.Expr.If(cond, ifTrue, ifFalse, position) => resolved.Expr.If(resolveExpr(cond), resolveExpr(ifTrue), resolveExpr(ifFalse), position)
    case raw.Expr.While(cond, body, position)         => resolved.Expr.While(resolveExpr(cond), resolveExpr(body), position)
    case raw.Expr.For(iterator, iterable, body, position) =>
      ResolutionContext.inNewScope(None)(
        resolved.Expr.For(
          ResolutionContext.declareTerm(Symbol.Variable(_, iterator, None, false, position)),
          resolveExpr(iterable),
          resolveExpr(body),
          position
        )
      )
    case raw.Expr.Invalid(position) => resolved.Expr.Invalid(position)

  /**
   * Resolve the given selector.
   *
   * @param qualifier the symbol owning the members to import
   * @param selector the member selector
   * @param qualifierPosition the source position of the owning symbol, used for error production
   */
  def resolveSelector(qualifier: SymbolId, selector: Selector, qualifierPosition: SourcePosition): Resolution[Unit] = selector match
    case Selector.Simple(name, position) =>
      ResolutionContext.importMember(qualifier, name, name, qualifierPosition, position)

    case Selector.Wildcard(position) =>
      get.symbols(qualifier) match
        case namespace: Namespace =>
          val namespaceScope = get.scopes(namespace.memberScope)
          ResolutionContext.updateCurrentScope(scope =>
            scope.copy(
              localTerms = scope.localTerms ++ namespaceScope.localTerms,
              localTypes = scope.localTypes ++ namespaceScope.localTypes
            )
          )
        case sym =>
          write(ResolutionError.NotANamespace(sym, qualifierPosition))

    case Selector.Rename(name, alias, position) =>
      ResolutionContext.importMember(qualifier, name, alias, qualifierPosition, position)

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
        write(ResolutionError.MultipleScriptFiles(SourcePosition.BuiltIn))
        fail(())
      else
        declaredPackages
          .tapEach:
            case (program, (packageId, packageScope)) => Resolver.declareProgram(program, packageId, packageScope)
          .map:
            case (program, (packageId, packageScope)) => Resolver.resolveProgram(program, packageId, packageScope)
