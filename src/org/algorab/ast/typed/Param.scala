package org.algorab.ast.typed

import org.algorab.ast.SymbolId
import org.algorab.util.SourcePosition

//TODO erase this to Param(symbol, tpe) (aka remove varargs/default) during resolution
enum Param:
  case Simple(symbol: SymbolId, tpe: Type, default: Option[Expr], position: SourcePosition)
  case Varargs(symbol: SymbolId, tpe: Type, position: SourcePosition)

  def symbol: SymbolId

  def usageType: Type = this match
    case Simple(_, tpe, _, _) => tpe
    case Varargs(_, tpe, _) => Type.Any
  