package org.algorab.runtime

import io.github.iltotore.pureparser.Span
import org.algorab.ast.Value
import org.algorab.ast.typed.Type
import org.algorab.typing.TypePattern
import org.algorab.util.ConsoleError

/**
 * An error occurring during the runtime phase.
 */
enum RuntimeError:

  /**
   * The given value does not match the expected type.
   *
   * @param expected the expected type pattern
   * @param got the actual value
   * @param span the source position where the error occurred
   */
  case TypeMismatch(expected: TypePattern, got: Value, span: Span)

  /**
   * An error occurred while reading from the console.
   *
   * @param error the console error
   * @param span the source position where the error occurred
   */
  case Console(error: ConsoleError, span: Span)

  /**
   * The source position where the error occurred.
   */
  def span: Span

object RuntimeError:

  /**
   * A type mismatch between an actual value and an expected type.
   *
   * @param expected the expected type
   * @param got the actual value
   * @param span the source position where the error occurred
   */
  def simpleMismatch(expected: Type, got: Value, span: Span): RuntimeError =
    RuntimeError.TypeMismatch(TypePattern.Type(expected), got, span)
