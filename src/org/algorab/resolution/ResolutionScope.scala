package org.algorab.resolution

import org.algorab.ast.Identifier
import org.algorab.ast.SymbolId

/**
 * A local scope used during name resolution.
 *
 * @param owner the owner of this scope
 * @param localTerms the terms locally declared in this scope
 * @param localTypes the types locally declared in this scope
 */
case class ResolutionScope(
    owner: Option[SymbolId],
    localTerms: Map[Identifier, (SymbolId, Boolean)],
    localTypes: Map[Identifier, SymbolId]
):

  /**
   * Locally declare a term in this scope.
   *
   * @param name the local name of the term
   * @param term the term to declare
   * @param initialized whether or not this term has been initialized
   * @return a copy of this scope with the new term
   */
  def withLocalTerm(name: Identifier, term: SymbolId, initialized: Boolean): ResolutionScope =
    this.copy(localTerms = localTerms.updated(name, (term, initialized)))

  /**
   * Locally declare a type in this scope.
   *
   * @param name the local name of the type
   * @param tpe the type to declare
   * @return a copy of this scope with the new type
   */
  def withLocalType(name: Identifier, tpe: SymbolId): ResolutionScope =
    this.copy(localTypes = localTypes.updated(name, tpe))

  /**
   * Initialize the locally-declared term.
   *
   * @param name the local name of the term to initialize
   * @return a copy of this scope with the referenced term initialized
   */
  def withLocalTermInitialized(name: Identifier): ResolutionScope =
    this.withLocalTerm(name, localTerms(name)._1, true)

object ResolutionScope:

  /**
   * Create an empty scope.
   *
   * @param owner the optional owner of this scope
   * @return a new scope with no declaration
   */
  def empty(owner: Option[SymbolId]): ResolutionScope = ResolutionScope(owner, Map.empty, Map.empty)
