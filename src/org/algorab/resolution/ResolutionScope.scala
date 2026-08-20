package org.algorab.resolution

import org.algorab.ast.SymbolId
import org.algorab.ast.Identifier

case class ResolutionScope(
  owner: Option[SymbolId],
  localTerms: Map[Identifier, (SymbolId, Boolean)],
  localTypes: Map[Identifier, SymbolId]
):

  def withLocalTerm(name: Identifier, term: SymbolId, initialized: Boolean): ResolutionScope =
    this.copy(localTerms = localTerms.updated(name, (term, initialized)))

  def withLocalType(name: Identifier, tpe: SymbolId): ResolutionScope =
    this.copy(localTypes = localTypes.updated(name, tpe))

  def withLocalTermInitialized(name: Identifier): ResolutionScope =
    this.withLocalTerm(name, localTerms(name)._1, true)

object ResolutionScope:

  def empty(owner: Option[SymbolId]): ResolutionScope = ResolutionScope(owner, Map.empty, Map.empty)