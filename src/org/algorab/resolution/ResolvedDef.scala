package org.algorab.resolution

import io.github.iltotore.pureparser.Span
import org.algorab.ast.Identifier

case class ResolvedDef(name: Identifier, span: Span, initialized: Boolean)
