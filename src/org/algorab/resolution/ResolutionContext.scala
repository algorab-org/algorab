package org.algorab.resolution

import org.algorab.ast.Identifier

case class ResolutionContext(scopes: List[ResolutionScope]):

  def getResolvedName(name: Identifier): Option[Identifier] =
    scopes.collectFirst(((_: ResolutionScope).resolvedNames.get(name)).unlift)

object ResolutionContext:

  val default: ResolutionContext = ResolutionContext(List(
    ResolutionScope(Identifier("root"), Map.empty, 0)
  ))