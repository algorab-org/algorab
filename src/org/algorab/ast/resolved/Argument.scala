package org.algorab.ast.resolved

import org.algorab.ast.SymbolId

enum Argument:
  case Positional(expr: Expr, variadic: Boolean)

  def expr: Expr

  def variadic: Boolean