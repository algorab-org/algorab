package org.algorab.resolution

import org.algorab.ast.Identifier
import org.algorab.ast.ResolvedId

case class ResolutionScope(scopeName: String, resolvedNames: Map[Identifier, Identifier], scopeCount: Int)