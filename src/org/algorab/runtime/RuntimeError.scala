package org.algorab.runtime

import org.algorab.ast.typed.Type
import io.github.iltotore.pureparser.Span
import org.algorab.typing.TypePattern
import org.algorab.ast.Value
import org.algorab.util.ConsoleError

enum RuntimeError:
  case TypeMismatch(expected: TypePattern, got: Value, span: Span)
  case Console(error: ConsoleError, span: Span)

  def span: Span

object RuntimeError:

  def simpleMismatch(expected: Type, got: Value, span: Span): RuntimeError =
    RuntimeError.TypeMismatch(TypePattern.Type(expected), got, span)