package org.algorab.resolution

import io.github.iltotore.iron.autoRefine
import io.github.iltotore.pureparser.Span
import org.algorab.ast.*
import purelogic.*

case class ResolutionContext(
    scopePath: List[ScopeId],
    symbols: Map[SymbolId, Symbol],
    declarations: Map[SymbolId, resolved.Definition],
    scopes: Map[ScopeId, ResolutionScope],
    nextSymbolId: SymbolId,
    nextScopeId: ScopeId
):

  def currentScopeId: ScopeId = scopePath.head

  def updateScope(id: ScopeId)(f: ResolutionScope => ResolutionScope): ResolutionContext = this.copy(
    scopes = scopes.updated(id, f(scopes(id)))
  )

  def updateCurrentScope(f: ResolutionScope => ResolutionScope): ResolutionContext =
    updateScope(currentScopeId)(f)

  def declarePredefType(id: SymbolId, name: Identifier): ResolutionContext = this
    .updateCurrentScope(_.withLocalType(name, id))
    .copy(
      symbols = symbols.updated(
        nextSymbolId,
        Symbol.Type(
          id = id,
          name = name,
          owner = Some(SymbolId.Root),
          span = Span(0, 0)
        )
      ),
      nextSymbolId = nextSymbolId.max(id + 1)
    )

  def declarePredefVariable(id: SymbolId, name: Identifier): ResolutionContext = this
    .updateCurrentScope(_.withLocalTerm(name, id, true))
    .copy(
      symbols = symbols.updated(
        nextSymbolId,
        Symbol.Variable(
          id = id,
          name = name,
          owner = Some(SymbolId.Root),
          mutable = false,
          span = Span(0, 0)
        )
      ),
      nextSymbolId = nextSymbolId.max(id + 1)
    )

  def declarePredefFunction(id: SymbolId, name: Identifier): ResolutionContext = this
    .updateCurrentScope(_.withLocalTerm(name, id, true))
    .copy(
      symbols = symbols.updated(
        nextSymbolId,
        Symbol.Function(
          id = id,
          name = name,
          owner = Some(SymbolId.Root),
          span = Span(0, 0)
        )
      ),
      nextSymbolId = nextSymbolId.max(id + 1)
    )

object ResolutionContext:

  val default: ResolutionContext = ResolutionContext(
    scopePath = List(ScopeId.Root),
    symbols = Map(
      SymbolId.Invalid -> Symbol.Invalid,
      SymbolId.Root -> Symbol.Root
    ),
    declarations = Map.empty,
    scopes = Map(ScopeId.Root -> ResolutionScope.empty(Some(SymbolId.Root))),
    nextSymbolId = SymbolId(1),
    nextScopeId = ScopeId(1)
  )
    .declarePredefType(SymbolId.AnyType, Identifier("Any"))
    .declarePredefType(SymbolId.UnitType, Identifier("Unit"))
    .declarePredefType(SymbolId.BooleanType, Identifier("Boolean"))
    .declarePredefType(SymbolId.IntType, Identifier("Int"))
    .declarePredefType(SymbolId.FloatType, Identifier("Float"))
    .declarePredefType(SymbolId.CharType, Identifier("Char"))
    .declarePredefType(SymbolId.StringType, Identifier("String"))
    .declarePredefVariable(SymbolId.UnitTerm, Identifier("Unit"))
    .declarePredefFunction(SymbolId.ToFloatTerm, Identifier("toFloat"))
    .declarePredefFunction(SymbolId.PrintLnTerm, Identifier("println"))
    .declarePredefFunction(SymbolId.ReadIntTerm, Identifier("readInt"))
    .declarePredefFunction(SymbolId.ReadFloatTerm, Identifier("readFloat"))

  def getOwner(id: SymbolId): Resolution[Option[SymbolId]] =
    get.symbols(id).owner

  def currentScope: Resolution[ResolutionScope] = get.scopes(get.currentScopeId)

  def updateCurrentScope(f: ResolutionScope => ResolutionScope): Resolution[Unit] =
    update(_.updateCurrentScope(f))

  def findInScopes[A](f: ResolutionScope => Option[A]): Resolution[Option[A]] =
    get.scopePath.collectFirst((get.scopes.apply andThen f).unlift)

  def getLocalTerm(name: Identifier, span: Span): Resolution[SymbolId] =
    findInScopes(_.localTerms.get(name)) match
      case Some((id, initialized)) =>
        if !initialized then write(ResolutionError.ForwardDeclaration(get.symbols(id), span))
        id
      case None =>
        write(ResolutionError.UnknownName(name, span))
        SymbolId.Invalid

  def getLocalType(name: Identifier, span: Span): Resolution[SymbolId] =
    findInScopes(_.localTypes.get(name)) match
      case Some(id) => id
      case None =>
        write(ResolutionError.UnknownName(name, span))
        SymbolId.Invalid

  def declareSymbol(symbol: Symbol.Valid): Resolution[Symbol.Valid] =
    val context = get
    set(context.copy(
      symbols = context.symbols.updated(context.nextSymbolId, symbol),
      nextSymbolId = context.nextSymbolId + 1
    ))
    symbol

  def declareLocalSymbol(symbol: Symbol.Valid): Resolution[Symbol.Valid] =
    declareSymbol(currentScope.owner.fold(symbol)(symbol.withOwner))

  def declareTerm(symbol: SymbolId => Symbol.Valid, initialized: Boolean = true): Resolution[SymbolId] =
    val id = get.nextSymbolId
    val undeclared = symbol(id)
    currentScope.localTerms.get(undeclared.name) match
      case Some((original, _)) =>
        write(ResolutionError.AlreadyDeclared(get.symbols(original), undeclared.span))
        original
      case None =>
        val sym = declareLocalSymbol(undeclared)
        updateCurrentScope(_.withLocalTerm(sym.name, id, initialized))
        id

  def declareType(symbol: SymbolId => Symbol.Valid): Resolution[SymbolId] =
    val id = get.nextSymbolId
    val undeclared = symbol(id)
    currentScope.localTypes.get(undeclared.name) match
      case Some(original) =>
        write(ResolutionError.AlreadyDeclared(get.symbols(original), undeclared.span))
        original
      case None =>
        val sym = declareLocalSymbol(undeclared)
        updateCurrentScope(_.withLocalType(sym.name, id))
        id

  def assignDeclaration(symbol: SymbolId)(declaration: resolved.Definition): Resolution[resolved.Definition] =
    update(ctx => ctx.copy(declarations = ctx.declarations.updated(symbol, declaration)))
    declaration

  def initializeLocalTerm(name: Identifier): Resolution[Unit] =
    updateCurrentScope(_.withLocalTermInitialized(name))

  def inNewScope[A](owner: Option[SymbolId])(body: Resolution[A]): Resolution[A] =
    val currentOwner = currentScope.owner
    update(context => context.copy(
      scopePath = context.nextScopeId :: context.scopePath,
      scopes = context.scopes.updated(context.nextScopeId, ResolutionScope.empty(currentOwner.flatMap(_ => owner))),
      nextScopeId = context.nextScopeId + 1
    ))
    val result = body
    update(context => context.copy(scopePath = context.scopePath.tail))
    result

  def inScopePath[A](scopePath: List[ScopeId])(body: Resolution[A]): Resolution[A] =
    val currentPath = get.scopePath
    update(_.copy(scopePath = scopePath))
    val result = body
    update(_.copy(scopePath = currentPath))
    result