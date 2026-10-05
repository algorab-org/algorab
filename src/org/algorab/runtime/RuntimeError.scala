package org.algorab.runtime

import org.algorab.ast.Value
import org.algorab.ast.SymbolId
import org.algorab.ast.Symbol
import org.algorab.ast.typed.Type
import org.algorab.typing.TypePattern
import org.algorab.util.ConsoleError
import org.algorab.util.SourcePosition
import org.algorab.AlgorabError
import org.algorab.show.Show
import org.algorab.show.Printer

/**
 * An error occurring during the runtime phase.
 */
enum RuntimeError extends AlgorabError:

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

  def message: Show[String] = this match
    case TypeMismatch(expected, got, _) =>
      s"""Type mismatch.
         |
         |Expected: ${Printer.showTypePattern(expected)}
         |Got: $got"""

    case Console(error, _) => error.message

  override def show: Show[String] = message

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
