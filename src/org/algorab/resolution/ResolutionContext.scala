package org.algorab.resolution

import io.github.iltotore.iron.autoRefine
import io.github.iltotore.pureparser.Span
import org.algorab.ast.Identifier
import org.algorab.ast.QualifiedName
import org.algorab.ast.Symbol
import org.algorab.ast.SymbolId
import purelogic.*

case class ResolutionContext(
    owner: Option[SymbolId],
    localTerms: Map[Identifier, ResolvedDef],
    localTypes: Map[Identifier, SymbolId],
    symbols: Map[SymbolId, Symbol],
    nextId: SymbolId
):

  def declarePredefType(name: Identifier): ResolutionContext = this.copy(
    localTypes = localTypes.updated(name, nextId),
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
    localTerms = localTerms.updated(name, ResolvedDef(nextId, initialized = true)),
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
    localTerms = localTerms.updated(name, ResolvedDef(nextId, initialized = true)),
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
    owner = Some(SymbolId.Root),
    localTerms = Map.empty,
    localTypes = Map.empty,
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

  def getLocalTerm(name: Identifier, span: Span): Resolution[SymbolId] =
    get.localTerms.get(name) match
      case Some(ResolvedDef(id, initialized)) =>
        if !initialized then write(ResolutionError.ForwardDeclaration(get.symbols(id), span))
        id
      case None =>
        write(ResolutionError.UnknownName(name, span))
        SymbolId.Invalid

  def getLocalType(name: Identifier, span: Span): Resolution[SymbolId] =
    get.localTypes.get(name) match
      case Some(id) => id
      case None =>
        write(ResolutionError.UnknownName(name, span))
        SymbolId.Invalid

  def declareSymbol(symbol: SymbolId => Symbol.Valid): Resolution[(SymbolId, Symbol.Valid)] =
    val context = get
    val sym = symbol(context.nextId)
    val withOwner = context.owner.fold(sym)(sym.withOwner)
    set(context.copy(
      symbols = context.symbols.updated(context.nextId, withOwner),
      nextId = context.nextId + 1
    ))
    (context.nextId, withOwner)

  def declareTerm(symbol: SymbolId => Symbol.Valid, initialized: Boolean = true): Resolution[SymbolId] =
    val (id, sym) = declareSymbol(symbol)
    update(context => context.copy(localTerms = context.localTerms.updated(sym.name, ResolvedDef(id, initialized))))
    id

  def declareType(symbol: SymbolId => Symbol.Valid): Resolution[SymbolId] =
    val (id, sym) = declareSymbol(symbol)
    update(context => context.copy(localTypes = context.localTypes.updated(sym.name, id)))
    id

  def markInitialized(name: Identifier): Resolution[Unit] =
    update(context =>
      context.copy(
        localTerms = context.localTerms.updated(name, context.localTerms(name).copy(initialized = true))
      )
    )

  def inNewScope[A](owner: Option[SymbolId])(body: Resolution[A]): Resolution[A] =
    val currentOwner = get.owner
    val localTerms = get.localTerms
    val localTypes = get.localTypes
    update(_.copy(owner = currentOwner.flatMap(_ => owner)))
    val result = body
    update(_.copy(owner = currentOwner, localTerms = localTerms, localTypes = localTypes))
    result
