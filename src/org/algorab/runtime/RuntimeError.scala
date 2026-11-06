package org.algorab.runtime

import fansi.Str
import org.algorab.AlgorabError
import org.algorab.ast.Symbol
import org.algorab.ast.SymbolId
import org.algorab.ast.Value
import org.algorab.ast.typed.Type
import org.algorab.show.Printer
import org.algorab.show.Show
import org.algorab.typing.TypePattern
import org.algorab.util.ConsoleError
import org.algorab.util.SourcePosition

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

  case IndexOutOfBound(size: Int, index: Int, position: SourcePosition)

  /**
   * The source position where the error occurred.
   */
  def position: SourcePosition

  /**
   * The error message.
   */
  def message: Show[String] = this match
    case TypeMismatch(expected, got, _) =>
      s"""Type mismatch.
         |
         |Expected: ${Printer.showTypePattern(expected)}
         |Got: $got""".stripMargin

    case Console(error, _) => error.message

    case IndexOutOfBound(size, index, _) =>
      s"""Index out of bound.
         |
         |Size: $size
         |Index: $index""".stripMargin

  override def show: Show[Str] = message

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
