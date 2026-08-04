package org.algorab.resolution

import org.algorab.ast.SymbolId
import org.algorab.ast.Identifier

case class ResolutionScope(
  owner: Option[SymbolId],
  localTerms: Map[Identifier, SymbolId],
  localTypes: Map[Identifier, SymbolId]
):

  def withLocalTerm(name: Identifier, term: SymbolId): ResolutionScope =
    this.copy(localTerms = localTerms.updated(name, term))

  def withLocalType(name: Identifier, tpe: SymbolId): ResolutionScope =
    this.copy(localTypes = localTypes.updated(name, tpe))

object ResolutionScope:

  def empty(owner: Option[SymbolId]): ResolutionScope = ResolutionScope(owner, Map.empty, Map.empty)