package org.algorab.ast.typed

import org.algorab.ast.SymbolId

/**
 * A parsed type.
 */
enum Type derives CanEqual:

  /**
   * A reference to a declared class.
   *
   * @param symbol the unique id of the referenced class
   */
  case Class(symbol: SymbolId)

  /**
   * The type of a function.
   *
   * @param inputs the type of the function's parameters
   * @param output the return type of the function
   */
  case Function(inputs: List[Type], output: Type)

  /**
   * An inferred type aka not explicitly typed in the source.
   */
  case Invalid

object Type:

  val Any: Type = Type.Class(SymbolId.AnyType)
  val Unit: Type = Type.Class(SymbolId.UnitType)
  val Boolean: Type = Type.Class(SymbolId.BooleanType)
  val Int: Type = Type.Class(SymbolId.IntType)
  val Float: Type = Type.Class(SymbolId.FloatType)
  val Char: Type = Type.Class(SymbolId.CharType)
  val String: Type = Type.Class(SymbolId.StringType)
