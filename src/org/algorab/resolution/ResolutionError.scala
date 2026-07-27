package org.algorab.resolution

import io.github.iltotore.pureparser.Span
import org.algorab.ast.Identifier

enum ResolutionError:
  case UnknownName(name: Identifier, span: Span)
  case ForwardDeclaration(name: Identifier, declaredAt: Span, span: Span)

  def span: Span
