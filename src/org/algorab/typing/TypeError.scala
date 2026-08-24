package org.algorab.typing

import org.algorab.ast.typed.Type
import org.algorab.ast.SymbolId

enum TypeError:
  case Mismatch(expected: List[TypePattern], got: List[Type])
  case ApplyOnNonFunction(got: Type)
  case ApplyMismatch(expectedParams: List[Type], got: List[Type])

object TypeError:

  def simpleMismatch(expected: List[Type], got: Type): TypeError = TypeError.Mismatch(
    expected = expected.map(TypePattern.Type.apply),
    got = List(got)
  )