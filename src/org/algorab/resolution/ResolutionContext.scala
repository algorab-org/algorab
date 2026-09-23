package org.algorab.resolution

import io.github.iltotore.iron.autoRefine
import io.github.iltotore.pureparser.Span
import org.algorab.ast.*
import purelogic.*

/**
 * The context used during the name resolution phase.
 *
 * @param scopePath the current lexical scope path
 * @param symbols the symbol table, linking id to the symbol metadata
 * @param declarations the declaration of each declared symbol, used for inference during the typing phase
 * @param scopes the scope table
 * @param nextSymbolId the id of the next symbol to declare
 * @param nextScopeId the id of the next scope to declare
 */
case class ResolutionContext(
    scopePath: List[ScopeId],
    symbols: Map[SymbolId, Symbol],
    declarations: Map[SymbolId, resolved.Definition],
    scopes: Map[ScopeId, ResolutionScope],
    nextSymbolId: SymbolId,
    nextScopeId: ScopeId
):

  /**
   * The id of the current lexical scope.
   */
  def currentScopeId: ScopeId = scopePath.head

  /**
   * Update a scope based on its id.
   *
   * @param id the id of the scope to update
   * @param f the update function to apply to the scope
   * @return a copy of this context with the scope updated
   */
  def updateScope(id: ScopeId)(f: ResolutionScope => ResolutionScope): ResolutionContext = this.copy(
    scopes = scopes.updated(id, f(scopes(id)))
  )

  /**
   * Update the most-direct scope the resolution phase is currently in, aka the current scope.
   *
   * @param f the update function to apply to the scope
   * @return a copy of this context with the scope updated
   */
  def updateCurrentScope(f: ResolutionScope => ResolutionScope): ResolutionContext =
    updateScope(currentScopeId)(f)

  /**
   * Declare a built-in type.
   *
   * @param id the id of the built-in symbol
   * @param name the name of the symbol to declare
   * @return a copy of this context with the new symbol
   */
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

  /**
   * Declare a built-in variable.
   *
   * @param id the id of the built-in symbol
   * @param name the name of the symbol to declare
   * @return a copy of this context with the new symbol
   */
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

  /**
   * Declare a built-in function.
   *
   * @param id the id of the built-in symbol
   * @param name the name of the symbol to declare
   * @return a copy of this context with the new symbol
   */
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

  /**
   * The default resolution context used during name resolution.
   * Contains standard symbols.
   */
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

  /**
   * Get the owner of the given symbol.
   *
   * @param id the id of the symbol for which the owner is got
   * @return the owner of the symbol
   */
  def getOwner(id: SymbolId): Resolution[Option[SymbolId]] =
    get.symbols(id).owner

  /**
   * The current scope.
   */
  def currentScope: Resolution[ResolutionScope] = get.scopes(get.currentScopeId)

  /**
   * Update the most-direct scope the resolution phase is currently in, aka the current scope.
   *
   * @param f the update function to apply to the scope
   */
  def updateCurrentScope(f: ResolutionScope => ResolutionScope): Resolution[Unit] =
    update(_.updateCurrentScope(f))

  /**
   * Find a value in the scope path.
   *
   * @param f the function returning the looked-for value if it exists
   * @return the looked-for value if found
   */
  def findInScopes[A](f: ResolutionScope => Option[A]): Resolution[Option[A]] =
    get.scopePath.collectFirst((get.scopes.apply andThen f).unlift)

  /**
   * Get the local term corresponding to the given name.
   *
   * @param name the name of the term to look for
   * @param span the source position from where the search is called, used for error reporting
   * @return the found local term
   */
  def getLocalTerm(name: Identifier, span: Span): Resolution[SymbolId] =
    findInScopes(_.localTerms.get(name)) match
      case Some((id, initialized)) =>
        if !initialized then write(ResolutionError.ForwardDeclaration(get.symbols(id), span))
        id
      case None =>
        write(ResolutionError.UnknownName(name, span))
        SymbolId.Invalid

  /**
   * Get the local type corresponding to the given name.
   *
   * @param name the name of the type to look for
   * @param span the source position from where the search is called, used for error reporting
   * @return the found local type
   */
  def getLocalType(name: Identifier, span: Span): Resolution[SymbolId] =
    findInScopes(_.localTypes.get(name)) match
      case Some(id) => id
      case None =>
        write(ResolutionError.UnknownName(name, span))
        SymbolId.Invalid

  /**
   * Get a member term of a symbol.
   *
   * @param owner the symbol owning the term to get
   * @param member the member to get
   * @param ownerSpan the source position of the owning symbol, used for error production
   * @return the symbol's member if it exists
   */
  def getMemberTerm(owner: SymbolId, member: Identifier, ownerSpan: Span): Resolution[Option[SymbolId]] =
    get.symbols(owner) match
      case namespace: Symbol.Namespace =>
        get
          .scopes(namespace.memberScope)
          .localTerms
          .get(member)
          .map(_._1)

      case sym =>
        write(ResolutionError.NotANamespace(sym, ownerSpan))
        Some(SymbolId.Invalid)

  /**
   * Get a member type of a symbol.
   *
   * @param owner the symbol owning the type to get
   * @param member the member to get
   * @param ownerSpan the source position of the owning symbol, used for error production
   * @return the symbol's member if it exists
   */
  def getMemberType(owner: SymbolId, member: Identifier, ownerSpan: Span): Resolution[Option[SymbolId]] =
    get.symbols(owner) match
      case namespace: Symbol.Namespace =>
        get
          .scopes(namespace.memberScope)
          .localTypes
          .get(member)

      case sym =>
        write(ResolutionError.NotANamespace(sym, ownerSpan))
        Some(SymbolId.Invalid)

  /**
   * Get a member term of a symbol or fail with a [[ResolutionError.UnknownName]].
   *
   * @param owner the symbol owning the term to get
   * @param member the member to get
   * @param ownerSpan the source position of the owning symbol, used for error production
   * @param memberSpan the source position of the member reference, used for error production
   * @return the symbol's member or [[SymbolId.Invalid]]
   */
  def getMemberTermOrFail(owner: SymbolId, member: Identifier, ownerSpan: Span, memberSpan: Span): Resolution[SymbolId] =
    getMemberTerm(owner, member, ownerSpan).getOrElse:
      write(ResolutionError.UnknownName(member, memberSpan))
      SymbolId.Invalid

  /**
   * Get a member type of a symbol or fail with a [[ResolutionError.UnknownName]].
   *
   * @param owner the symbol owning the type to get
   * @param member the member to get
   * @param ownerSpan the source position of the owning symbol, used for error production
   * @param memberSpan the source position of the member reference, used for error production
   * @return the symbol's member or [[SymbolId.Invalid]]
   */
  def getMemberTypeOrFail(owner: SymbolId, member: Identifier, ownerSpan: Span, memberSpan: Span): Resolution[SymbolId] =
    getMemberType(owner, member, ownerSpan).getOrElse:
      write(ResolutionError.UnknownName(member, memberSpan))
      SymbolId.Invalid

  /**
   * Import a member in the current scope.
   * In case both a type a term have the same name, both are imported.
   *
   * @param owner the symbol owning the member to import
   * @param member the name of the member to import
   * @param alias the name the member is imported as
   * @param ownerSpan the source position of the owning symbol, used for error production
   * @param memberSpan the source position of the member reference, used for error production
   */
  def importMember(owner: SymbolId, member: Identifier, alias: Identifier, ownerSpan: Span, memberSpan: Span): Resolution[Unit] =
    val memberTerm = ResolutionContext.getMemberTerm(owner, member, ownerSpan)
    val memberType = ResolutionContext.getMemberType(owner, member, ownerSpan)
    if memberTerm.isEmpty && memberType.isEmpty then
      write(ResolutionError.UnknownName(member, memberSpan))
    else
      ResolutionContext.updateCurrentScope(scope =>
        val withTerm = memberTerm
          .filterNot(_ => scope.localTerms.contains(alias))
          .fold(scope)(scope.withLocalTerm(alias, _, true))
        val res = memberType
          .filterNot(_ => scope.localTypes.contains(alias))
          .fold(withTerm)(withTerm.withLocalType(alias, _))
        res
      )

  /**
   * Declare the given symbol.
   *
   * @param symbol the symbol to add to the scope table
   * @return the given symbol
   */
  def declareSymbol(symbol: Symbol.Valid): Resolution[Symbol.Valid] =
    val context = get
    set(context.copy(
      symbols = context.symbols.updated(context.nextSymbolId, symbol),
      nextSymbolId = context.nextSymbolId + 1
    ))
    symbol

  /**
   * Declare the given symbol locally.
   *
   * @param symbol the symbol to add to the scope table, assigning the current scope's owner as its owner
   * @return the given symbol with its new owner
   */
  def declareLocalSymbol(symbol: Symbol.Valid): Resolution[Symbol.Valid] =
    declareSymbol(currentScope.owner.fold(symbol)(symbol.withOwner))

  /**
   * Declare the given term locally, also adding it using its name to the current term scope.
   *
   * @param symbol the symbol to add to the scope table, assigning the current scope's owner as its owner
   * @param initialized whether or not this symbol is initialized
   * @return the id assigned to this symbol
   */
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

  /**
   * Declare the given type locally, also adding it using its name to the current type scope.
   *
   * @param symbol the symbol to add to the scope table, assigning the current scope's owner as its owner
   * @return the id assigned to this symbol
   */
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

  /**
   * Assign a declaration to its symbol.
   *
   * @param symbol the id of the declared symbol
   * @param declaration the declaration to assign to the symbol
   * @return the given declaration
   */
  def assignDeclaration(symbol: SymbolId)(declaration: resolved.Definition): Resolution[resolved.Definition] =
    update(ctx => ctx.copy(declarations = ctx.declarations.updated(symbol, declaration)))
    declaration

  /**
   * Initialize the local term declared under the given name.
   *
   * @param name the name of the term to initialize
   */
  def initializeLocalTerm(name: Identifier): Resolution[Unit] =
    updateCurrentScope(_.withLocalTermInitialized(name))

  /**
   * Evaluate the given body in a new scope which itself is owned by the given owner.
   *
   * @param owner the optional owner of the new scope
   * @param body the body to evaluate
   * @return the result of the evaluation
   */
  def inNewScope[A](owner: Option[SymbolId])(body: Resolution[A]): Resolution[A] =
    val currentOwner = currentScope.owner
    update(context =>
      context.copy(
        scopePath = context.nextScopeId :: context.scopePath,
        scopes = context.scopes.updated(context.nextScopeId, ResolutionScope.empty(currentOwner.flatMap(_ => owner))),
        nextScopeId = context.nextScopeId + 1
      )
    )
    val result = body
    update(context => context.copy(scopePath = context.scopePath.tail))
    result

  /**
   * Evaluate the given body under the given scope path.
   *
   * @param scopePath the scopes to use to evaluate this body
   * @param body the body to evaluate
   * @return the result of the evaluation
   */
  def inScopePath[A](scopePath: List[ScopeId])(body: Resolution[A]): Resolution[A] =
    val currentPath = get.scopePath
    update(_.copy(scopePath = scopePath))
    val result = body
    update(_.copy(scopePath = currentPath))
    result
