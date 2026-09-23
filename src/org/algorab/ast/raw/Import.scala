package org.algorab.ast.raw

import org.algorab.ast.raw.Import.Selector
import org.algorab.ast.Identifier
import io.github.iltotore.pureparser.Span

case class Import(path: List[(Identifier, Span)], selector: Selector, span: Span)

object Import:

  enum Selector:
    case Simple(name: Identifier, span: Span)

    case Wildcard(span: Span)

    case Rename(name: Identifier, alias: Identifier, span: Span)

    def span: Span