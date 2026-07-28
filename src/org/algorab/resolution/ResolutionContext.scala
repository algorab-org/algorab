package org.algorab.resolution

import io.github.iltotore.iron.autoRefine
import io.github.iltotore.pureparser.Span
import org.algorab.ast.Identifier
import org.algorab.ast.QualifiedName
import org.algorab.ast.Symbol
import org.algorab.ast.SymbolId
import purelogic.*

case class ResolutionContext(
    scopeName: Option[QualifiedName],
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
        qualifiedName = Some(QualifiedName.assume(name.value)),
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
        qualifiedName = Some(QualifiedName.assume(name.value)),
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
        qualifiedName = Some(QualifiedName.assume(name.value)),
        span = Span(0, 0)
      )
    ),
    nextId = nextId + 1
  )

object ResolutionContext:

  val default: ResolutionContext = ResolutionContext(
    scopeName = Some(QualifiedName("root")),
    localTerms = Map.empty,
    localTypes = Map.empty,
    symbols = Map(SymbolId.Invalid -> Symbol.Invalid),
    nextId = SymbolId(0)
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

  def getQualifiedName(id: SymbolId): Resolution[Option[QualifiedName]] =
    get.symbols(id) match
      case valid: Symbol.Valid => valid.qualifiedName
      case _ => throw AssertionError(s"Get qualified name of invalid symbol $id")

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
    val qualifiedSymbol = context.scopeName.fold(sym)(name =>
      sym.withQualifiedName(QualifiedName(s"$name.${sym.name}"))
    )
    set(context.copy(
      symbols = context.symbols.updated(context.nextId, qualifiedSymbol),
      nextId = context.nextId + 1
    ))
    (context.nextId, qualifiedSymbol)

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

  def inNewScope[A](name: Option[QualifiedName])(body: Resolution[A]): Resolution[A] =
    val scopeName = get.scopeName
    val localTerms = get.localTerms
    val localTypes = get.localTypes
    update(_.copy(scopeName = name))
    val result = body
    update(_.copy(scopeName = scopeName, localTerms = localTerms, localTypes = localTypes))
    result
