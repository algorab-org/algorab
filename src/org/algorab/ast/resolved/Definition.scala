package org.algorab.ast.resolved

import io.github.iltotore.pureparser.Span
import org.algorab.ast.SymbolId

enum Definition:
  case Val(symbol: SymbolId, tpe: Type, expr: Expr, mutable: Boolean, span: Span)
  case Function(symbol: SymbolId, params: List[(SymbolId, Type)], retType: Type, body: Expr, span: Span)

  def span: Span
