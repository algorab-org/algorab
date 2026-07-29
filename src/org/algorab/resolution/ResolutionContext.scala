package org.algorab.resolution

import io.github.iltotore.iron.autoRefine
import io.github.iltotore.pureparser.Span
import org.algorab.ast.Identifier
import org.algorab.ast.QualifiedName
import org.algorab.ast.Symbol
import org.algorab.ast.SymbolId
import purelogic.*

case class ResolutionContext(
    scopes: List[ResolutionScope],
    symbols: Map[SymbolId, Symbol],
    nextId: SymbolId
):

  def declarePredefType(name: Identifier): ResolutionContext = this.copy(
    scopes = scopes.head.withLocalType(name, nextId) :: scopes.tail,
    symbols = symbols.updated(
      nextId,
      Symbol.Type(
        id = nextId,
        name = name,
        owner = Some(SymbolId.Root),
        span = Span(0, 0)
      )
    ),
    nextId = nextId + 1
  )

  def declarePredefVariable(name: Identifier): ResolutionContext = this.copy(
    scopes = scopes.head.withLocalTerm(name, ResolvedDef(nextId, initialized = true)) :: scopes.tail,
    symbols = symbols.updated(
      nextId,
      Symbol.Variable(
        id = nextId,
        name = name,
        owner = Some(SymbolId.Root),
        mutable = false,
        span = Span(0, 0)
      )
    ),
    nextId = nextId + 1
  )

  def declarePredefFunction(name: Identifier): ResolutionContext = this.copy(
    scopes = scopes.head.withLocalTerm(name, ResolvedDef(nextId, initialized = true)) :: scopes.tail,
    symbols = symbols.updated(
      nextId,
      Symbol.Function(
        id = nextId,
        name = name,
        owner = Some(SymbolId.Root),
        span = Span(0, 0)
      )
    ),
    nextId = nextId + 1
  )

object ResolutionContext:

  val default: ResolutionContext = ResolutionContext(
    scopes = List(ResolutionScope.empty(Some(SymbolId.Root))),
    symbols = Map(
      SymbolId.Invalid -> Symbol.Invalid,
      SymbolId.Root -> Symbol.Root
    ),
    nextId = SymbolId(1)
  )
    .declarePredefType(Identifier("Unit"))
    .declarePredefType(Identifier("Boolean"))
    .declarePredefType(Identifier("Int"))
    .declarePredefType(Identifier("Float"))
    .declarePredefType(Identifier("Char"))
    .declarePredefType(Identifier("String"))
    .declarePredefVariable(Identifier("Unit"))
    .declarePredefFunction(Identifier("println"))
    .declarePredefFunction(Identifier("readInt"))
    .declarePredefFunction(Identifier("readFloat"))

  def getOwner(id: SymbolId): Resolution[Option[SymbolId]] =
    get.symbols(id).owner

  def currentScope: Resolution[ResolutionScope] = get.scopes.head

  def updateCurrentScope(f: ResolutionScope => ResolutionScope): Resolution[Unit] =
    update(context => context.copy(scopes = f(context.scopes.head) :: context.scopes.tail))

  def getLocalTerm(name: Identifier, span: Span): Resolution[SymbolId] =
    get.scopes.collectFirst(((_: ResolutionScope).localTerms.get(name)).unlift) match
      case Some(ResolvedDef(id, initialized)) =>
        if !initialized then write(ResolutionError.ForwardDeclaration(get.symbols(id), span))
        id
      case None =>
        write(ResolutionError.UnknownName(name, span))
        SymbolId.Invalid

  def getLocalType(name: Identifier, span: Span): Resolution[SymbolId] =
    get.scopes.collectFirst(((_: ResolutionScope).localTypes.get(name)).unlift) match
      case Some(id) => id
      case None =>
        write(ResolutionError.UnknownName(name, span))
        SymbolId.Invalid

  def declareSymbol(symbol: Symbol.Valid): Resolution[(Symbol.Valid)] =
    val withOwner = currentScope.owner.fold(symbol)(symbol.withOwner)
    val context = get
    set(context.copy(
      symbols = context.symbols.updated(context.nextId, withOwner),
      nextId = context.nextId + 1
    ))
    (withOwner)

  def declareTerm(symbol: SymbolId => Symbol.Valid, initialized: Boolean = true): Resolution[SymbolId] =
    val id = get.nextId
    val undeclared = symbol(id)
    currentScope.localTerms.get(undeclared.name) match
      case Some(ResolvedDef(original, _)) =>
        write(ResolutionError.AlreadyDeclared(get.symbols(original), undeclared.span))
        original
      case None =>
        val sym = declareSymbol(undeclared)
        updateCurrentScope(_.withLocalTerm(sym.name, ResolvedDef(id, initialized)))
        id

  def declareType(symbol: SymbolId => Symbol.Valid): Resolution[SymbolId] =
    val id = get.nextId
    val undeclared = symbol(id)
    currentScope.localTypes.get(undeclared.name) match
      case Some(original) =>
        write(ResolutionError.AlreadyDeclared(get.symbols(original), undeclared.span))
        original
      case None =>
        val sym = declareSymbol(undeclared)
        updateCurrentScope(_.withLocalType(sym.name, id))
        id

  def markInitialized(name: Identifier): Resolution[Unit] =
    val term = currentScope.localTerms(name)
    updateCurrentScope(_.withLocalTerm(name, term.copy(initialized = true)))

  def inNewScope[A](owner: Option[SymbolId])(body: Resolution[A]): Resolution[A] =
    val currentOwner = currentScope.owner
    update(context => context.copy(
      scopes = ResolutionScope.empty(currentOwner.flatMap(_ => owner)) :: context.scopes
    ))
    val result = body
    update(context => context.copy(scopes = context.scopes.tail))
    result
