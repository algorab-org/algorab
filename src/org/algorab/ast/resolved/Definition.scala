package org.algorab.ast.resolved

import org.algorab.ast.SymbolId
import org.algorab.util.SourcePosition

/**
 * A definition such as variable or function declaration.
 */
enum Definition:

  /**
   * A variable declaration.
   *
   * @param symbol the definition's unique id
   * @param tpe the variable's declared type
   * @param expr the variable's RHS
   * @param mutable whether or not this variable is mutable
   * @param position the source position of this definition
   */
  case Val(symbol: SymbolId, tpe: Type, expr: Expr, mutable: Boolean, position: SourcePosition)

  /**
   * A function declaration.
   *
   * @param symbol the definition's unique id
   * @param tpe the variable's declared type
   * @param expr the variable's RHS
   * @param mutable whether or not this variable is mutable
   * @param position the source position of this definition
   */
  case Function(symbol: SymbolId, params: List[(SymbolId, Type)], retType: Type, body: Expr, position: SourcePosition)

  /**
   * The definition's unique id.
   */
  def symbol: SymbolId

  /**
   * The definition's unique id.
   */
  def position: SourcePosition
