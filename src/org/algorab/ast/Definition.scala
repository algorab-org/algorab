package org.algorab.ast

import io.github.iltotore.pureparser.Span

enum Definition:
  case Val(name: Identifier, tpe: Type, expr: Expr, mutable: Boolean, span: Span)
  case Function(name: Identifier, params: List[(Identifier, Type)], retType: Type, body: Expr, span: Span)

  def span: Span
