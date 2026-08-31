package org.algorab.ast.raw

import io.github.iltotore.pureparser.Span
import org.algorab.ast.Identifier
import org.algorab.ast.raw.Expr
import org.algorab.ast.raw.Type

/**
 * A definition such as variable or function declaration.
 */
enum Definition:

  /**
   * A variable declaration.
   *
   * @param name the variable's name
   * @param tpe the variable's declared type
   * @param expr the variable's RHS
   * @param mutable whether or not this variable is mutable
   * @param span the source position of this definition
   */
  case Val(name: Identifier, tpe: Type, expr: Expr, mutable: Boolean, span: Span)

  /**
   * A function declaration.
   *
   * @param name the variable's name
   * @param tpe the variable's declared type
   * @param expr the variable's RHS
   * @param mutable whether or not this variable is mutable
   * @param span the source position of this definition
   */
  case Function(name: Identifier, params: List[(Identifier, Type)], retType: Type, body: Expr, span: Span)

  /**
   * The source position of this definition.
   */
  def span: Span
