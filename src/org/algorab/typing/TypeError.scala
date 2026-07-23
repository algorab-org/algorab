package org.algorab.typing

import org.algorab.ast.tpd.Type
import io.github.iltotore.pureparser.Span
import org.algorab.ast.tpd.Expr

enum TypeError:
  case Mismatch(expected: List[Type], got: Type, span: Span)

  val span: Span

object TypeError:
  
  def mismatchExpr(expected: List[Type], expr: Expr): TypeError =
    TypeError.Mismatch(expected, expr.tpe, expr.span)