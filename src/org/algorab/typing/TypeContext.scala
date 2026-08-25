package org.algorab.typing

import org.algorab.ast.SymbolId
import org.algorab.ast.Symbol
import org.algorab.ast.typed.Type
import purelogic.*
import org.algorab.ast.Identifier
import org.algorab.ast.resolved

case class TypeContext(
  symbols: Map[SymbolId, Symbol],
  declarations: Map[SymbolId, resolved.Definition],
  types: Map[SymbolId, Option[Type]]
)

object TypeContext:

  def default(symbols: Map[SymbolId, Symbol], declarations: Map[SymbolId, resolved.Definition]): TypeContext = TypeContext(
    symbols = symbols,
    declarations = declarations,
    types = Map(
      SymbolId.Invalid -> Some(Type.Invalid),
      SymbolId.UnitTerm -> Some(Type.Unit),
      SymbolId.ToFloatTerm -> Some(Type.Function(List(Type.Int), Type.Float)),
      SymbolId.PrintLnTerm -> Some(Type.Function(List(Type.Any), Type.Unit)),
      SymbolId.ReadIntTerm -> Some(Type.Function(Nil, Type.Int)),
      SymbolId.ReadFloatTerm -> Some(Type.Function(Nil, Type.Float))
    )
  )

  def isTyped(symbol: SymbolId): Typing[Boolean] = get.types.contains(symbol)

  def getType(symbol: SymbolId): Typing[Type] = get.types(symbol) match
    case Some(tpe) => tpe
    case None =>
      write(TypeError.RecursiveInference(get.declarations(symbol).span))
      assignType(symbol, Type.Invalid)
      Type.Invalid
  
  def assignType(symbol: SymbolId, tpe: Type): Typing[Unit] = update(ctx =>
    ctx.copy(types = ctx.types.updated(symbol, Some(tpe)))
  )

  def startInferring(symbol: SymbolId): Typing[Unit] = update(ctx =>
    ctx.copy(types = ctx.types.updated(symbol, None))  
  )

  def isSubtype(typeA: Type, typeB: Type): Typing[Boolean] =
    typeA == typeB || typeB == Type.Any

  def getSymbol(symbol: SymbolId): Typing[Symbol] = get.symbols(symbol)

  def getDeclaration(symbol: SymbolId): Typing[resolved.Definition] = get.declarations(symbol)