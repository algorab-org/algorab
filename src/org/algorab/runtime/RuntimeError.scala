package org.algorab.runtime

import org.algorab.ast.Value
import org.algorab.ast.typed.Type
import org.algorab.typing.TypePattern
import org.algorab.util.ConsoleError
import org.algorab.util.SourcePosition

/**
 * An error occurring during the runtime phase.
 */
enum RuntimeError:

  /**
   * The given value does not match the expected type.
   *
   * @param expected the expected type pattern
   * @param got the actual value
   * @param position the source position where the error occurred
   */
  case TypeMismatch(expected: TypePattern, got: Value, position: SourcePosition)

  /**
   * An error occurred while reading from the console.
   *
   * @param error the console error
   * @param position the source position where the error occurred
   */
  case Console(error: ConsoleError, position: SourcePosition)

  /**
   * The source position where the error occurred.
   */
  def position: SourcePosition

object RuntimeError:

  /**
   * A type mismatch between an actual value and an expected type.
   *
   * @param expected the expected type
   * @param got the actual value
   * @param position the source position where the error occurred
   */
  def simpleMismatch(expected: Type, got: Value, position: SourcePosition): RuntimeError =
    RuntimeError.TypeMismatch(TypePattern.Type(expected), got, position)
