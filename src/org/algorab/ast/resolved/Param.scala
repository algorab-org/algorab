package org.algorab.ast.resolved

import org.algorab.ast.SymbolId
import org.algorab.util.SourcePosition

enum Param:
  case Simple(symbol: SymbolId, tpe: Type, default: Option[Expr], position: SourcePosition)
  case Varargs(symbol: SymbolId, tpe: Type, position: SourcePosition)

  def symbol: SymbolId