package org.algorab.ast.raw

import io.github.iltotore.pureparser.Span
import org.algorab.ast.raw.Type
import org.algorab.ast.raw.Expr
import org.algorab.ast.Identifier

enum Definition:
  case Val(name: Identifier, tpe: Type, expr: Expr, mutable: Boolean, span: Span)
  case Function(name: Identifier, params: List[(Identifier, Type)], retType: Type, body: Expr, span: Span)

  def span: Span
