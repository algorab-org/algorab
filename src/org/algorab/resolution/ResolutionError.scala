package org.algorab.resolution

import io.github.iltotore.pureparser.Span
import org.algorab.ast.Identifier
import org.algorab.ast.Symbol

enum ResolutionError:
  case UnknownName(name: Identifier, span: Span)
  case ForwardDeclaration(symbol: Symbol, span: Span)
  case AlreadyDeclared(symbol: Symbol, span: Span)
  case NotANamespace(symbol: Symbol, span: Span)

  def span: Span
