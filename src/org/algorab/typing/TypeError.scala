package org.algorab.typing

import io.github.iltotore.pureparser.Span
import org.algorab.ast.SymbolId
import org.algorab.ast.typed.Type

/**
 * An error occurring during the typing phase.
 */
enum TypeError:

  /**
   * The given types do not match any of the expected ones.
   *
   * @param expected the valid type patterns
   * @param got the actual type
   * @param span the source position where the error occurred
   */
  case Mismatch(expected: List[TypePattern], got: List[Type], span: Span)

  /**
   * Tried to do a function application on something that is not applicable (not a function, nor a class...).
   *
   * @param got the actual type of the applied expression
   * @param span the source position where the error occurred
   */
  case ApplyOnNonFunction(got: Type, span: Span)

  /**
   * The passed arguments do not match the expected parameters.
   *
   * @param expectedParams the parameters to fill
   * @param got the passed arguments
   * @param span the source position where the error occurred
   */
  case ApplyMismatch(expectedParams: List[Type], got: List[Type], span: Span)

  /**
   * Tried to infer the type of a recursive definition.
   *
   * @param span the source position where the error occurred
   */
  case RecursiveInference(span: Span)

  /**
   * The source position where the error occurred.
   */
  def span: Span

object TypeError:

  /**
   * A type mispmatch between an actual type and a list of expected types.
   *
   * @param expected the valid types
   * @param got the actual type
   * @param span the source position where the error occurred
   */
  def simpleMismatch(expected: List[Type], got: Type, span: Span): TypeError = TypeError.Mismatch(
    expected = expected.map(TypePattern.Type.apply),
    got = List(got),
    span
  )
