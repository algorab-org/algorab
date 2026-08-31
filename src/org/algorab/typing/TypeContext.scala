package org.algorab.typing

import org.algorab.ast.Identifier
import org.algorab.ast.Symbol
import org.algorab.ast.SymbolId
import org.algorab.ast.resolved
import org.algorab.ast.typed.Type
import purelogic.*

/**
 * The context used during the typing phase.
 *
 * @param symbols the symbol table, linking id to the symbol metadata
 * @param declarations the declaration of each declared symbol, used for inference during the typing phase
 * @param types the type for each typed symbol, [[None]] meaning that the symbol is currently being type-inferred
 */
case class TypeContext(
    symbols: Map[SymbolId, Symbol],
    declarations: Map[SymbolId, resolved.Definition],
    types: Map[SymbolId, Option[Type]]
)

object TypeContext:

  /**
   * The default type context used during typing.
   * Contains standard symbols.
   *
   * @param symbols the symbol table, linking id to the symbol metadata
   * @param declarations the declaration of each declared symbol, used for inference during the typing phase
   * @return a new type context with standard symbols types plus the given symbols and declaration
   */
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

  /**
   * Check if a symbol is typed.
   *
   * @param symbol the id of the symbol to check
   * @return `true` if the symbol is typed/being typed
   */
  def isTyped(symbol: SymbolId): Typing[Boolean] = get.types.contains(symbol)

  /**
   * Get the type of the given symbol. Assumes the symbol is already typed or is being typed.
   * If the symbol is being type-inferred, a "recursive type inference" error is produced.
   *
   * @param symbol the id of the symbol
   * @return the type of the given symbol
   */
  def getType(symbol: SymbolId): Typing[Type] = get.types(symbol) match
    case Some(tpe) => tpe
    case None =>
      write(TypeError.RecursiveInference(get.declarations(symbol).span))
      Type.Invalid

  /**
   * Assign a type to a symbol.
   *
   * @param symbol the id of the symbol to assign the type to
   * @param tpe the type to assign to the symbol
   */
  def assignType(symbol: SymbolId, tpe: Type): Typing[Unit] = update(ctx =>
    ctx.copy(types = ctx.types.updated(symbol, Some(tpe)))
  )

  /**
   * Mark the given symbol as "currently being inferred".
   *
   * @param symbol the symbol to mark
   */
  def startInferring(symbol: SymbolId): Typing[Unit] = update(ctx =>
    ctx.copy(types = ctx.types.updated(symbol, None))
  )

  /**
   * Check if the first type is a subtype of the second.
   *
   * @param typeA the expected subtype
   * @param typeB the expected supertype
   * @return `true` if `typeA <: typeB`
   */
  def isSubtype(typeA: Type, typeB: Type): Typing[Boolean] =
    typeA == typeB || typeB == Type.Any

  /**
   * Get the metadata of a symbol.
   *
   * @param symbol the id of the symbol
   * @return the [[Symbol]] instance corresponding to the given symbol id
   */
  def getSymbol(symbol: SymbolId): Typing[Symbol] = get.symbols(symbol)

  /**
   * Get the declaration of a symbol.
   *
   * @param symbol the id of the symbol
   * @return the untyped definition of the symbol
   */
  def getDeclaration(symbol: SymbolId): Typing[resolved.Definition] = get.declarations(symbol)
