package org.algorab.typing

import fansi.Str
import org.algorab.AlgorabError
import org.algorab.ast.Symbol
import org.algorab.ast.SymbolId
import org.algorab.ast.typed.Type
import org.algorab.show.Printer
import org.algorab.show.Show
import org.algorab.show.str
import org.algorab.show.strStripMargin
import org.algorab.util.SourcePosition

/**
 * An error occurring during the typing phase.
 */
enum TypeError extends AlgorabError.Frontend:

  /**
   * The given types do not match any of the expected ones.
   *
   * @param expected the valid type patterns
   * @param got the actual type
   * @param position the source position where the error occurred
   */
  case Mismatch(expected: List[TypePattern], got: List[Type], position: SourcePosition)

  /**
   * Tried to do a function application on something that is not applicable (not a function, nor a class...).
   *
   * @param got the actual type of the applied expression
   * @param position the source position where the error occurred
   */
  case ApplyOnNonFunction(got: Type, position: SourcePosition)

  /**
   * The passed arguments do not match the expected parameters.
   *
   * @param expectedParams the parameters to fill
   * @param got the passed arguments
   * @param position the source position where the error occurred
   */
  case ApplyMismatch(expectedParams: List[Type], got: List[Type], position: SourcePosition)

  /**
   * Tried to infer the type of a recursive definition.
   *
   * @param position the source position where the error occurred
   */
  case RecursiveInference(position: SourcePosition)

  /**
   * OOP is not supported yet. Emitted when trying to select a field/method.
   *
   * @param position the source position where the error occurred
   */
  case UnsupportedOOP(position: SourcePosition)

  override def message: Show[Str] = this match
    case Mismatch(expected, got, _) =>
      val showExpected = expected match
        case Nil            => "nothing"
        case List(expected) => Printer.showTypePattern(expected)
        case _              => expected.map(Printer.showTypePattern).mkString("\n- ", "\n- ", "")

      str"""Type mismatch.
           |Expected: $showExpected
           |Got: ${got.map(Printer.showType).mkString(", ")}""".strStripMargin

    case ApplyOnNonFunction(got, _) =>
      str"""Function application such as `foo(...)` can only be done on a function or an array.
           |Got: ${Printer.showType(got)}""".strStripMargin

    case ApplyMismatch(expectedParams, got, _) =>
      str"""Parameter mismatch.
           |Expected parameters: ${expectedParams.map(Printer.showType).mkString(", ")}
           |Got: ${got.map(Printer.showType).mkString(", ")}""".strStripMargin
    case RecursiveInference(_) => "Recursive definition needs explicit type."
    case UnsupportedOOP(_)     => "Object-oriented programming (OOP) is not supported yet."

object TypeError:

  /**
   * A type mispmatch between an actual type and a list of expected types.
   *
   * @param expected the valid types
   * @param got the actual type
   * @param position the source position where the error occurred
   */
  def simpleMismatch(expected: List[Type], got: Type, position: SourcePosition): TypeError = TypeError.Mismatch(
    expected = expected.map(TypePattern.Type.apply),
    got = List(got),
    position
  )
