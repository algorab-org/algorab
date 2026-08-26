package org.algorab.ast.typed

import org.algorab.ast.SymbolId

enum Type derives CanEqual:
  case Class(symbol: SymbolId)
  case Function(inputs: List[Type], output: Type)
  case Invalid

object Type:

  val Any: Type = Type.Class(SymbolId.AnyType)
  val Unit: Type = Type.Class(SymbolId.UnitType)
  val Boolean: Type = Type.Class(SymbolId.BooleanType)
  val Int: Type = Type.Class(SymbolId.IntType)
  val Float: Type = Type.Class(SymbolId.FloatType)
  val Char: Type = Type.Class(SymbolId.CharType)
  val String: Type = Type.Class(SymbolId.StringType)