package org.algorab.typing

import org.algorab.ast.typed.Type
import org.algorab.ast.SymbolId
import io.github.iltotore.pureparser.Span

enum TypeError:
  case Mismatch(expected: List[TypePattern], got: List[Type], span: Span)
  case ApplyOnNonFunction(got: Type, span: Span)
  case ApplyMismatch(expectedParams: List[Type], got: List[Type], span: Span)
  case RecursiveInference(span: Span)

  def span: Span

object TypeError:

  def simpleMismatch(expected: List[Type], got: Type, span: Span): TypeError = TypeError.Mismatch(
    expected = expected.map(TypePattern.Type.apply),
    got = List(got),
    span
  )