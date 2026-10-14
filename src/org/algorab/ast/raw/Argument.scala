package org.algorab.ast.raw

import org.algorab.ast.Identifier

enum Argument:
  case Positional(expr: Expr, variadic: Boolean)