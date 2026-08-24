package org.algorab.typing

import org.algorab.ast.SymbolId
import org.algorab.ast.Symbol
import org.algorab.ast.typed.Type
import purelogic.*

case class TypeContext(symbols: Map[SymbolId, Symbol], types: Map[SymbolId, Type])

object TypeContext:

  def empty(symbols: Map[SymbolId, Symbol]): TypeContext = TypeContext(
    symbols = symbols,
    types = Map(
      SymbolId.Invalid -> Type.Invalid
    )
  )

  def getType(symbol: SymbolId): Typing[Type] = get.types(symbol)
  
  def assignType(symbol: SymbolId, tpe: Type): Typing[Unit] = update(ctx => ctx.copy(types = ctx.types.updated(symbol, tpe)))

  def isSubtype(typeA: Type, typeB: Type): Typing[Boolean] =
    typeA == typeB

  def getSymbol(symbol: SymbolId): Typing[Symbol] = get.symbols(symbol)