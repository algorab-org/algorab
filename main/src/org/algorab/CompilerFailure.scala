/**
 * Compiler-level failures that can be reported to the user.
 *
 * A [[CompilerFailure]] is either a [[kyo.ParseFailure]] produced during lexing/parsing, or a
 * [[typer.TypeFailure]] raised during type-checking.  Both alternatives are handled by the
 * [[toPrettyString]] extension method, which produces a human-readable diagnostic string
 * suitable for display on the command line.
 */
package org.algorab

import kyo.ParseFailure
import org.algorab.typer.TypeFailure

/** Union of all failure types that can be produced by the compiler front end. */
type CompilerFailure = ParseFailure | TypeFailure

extension (failure: CompilerFailure)
  /**
   * Formats a [[CompilerFailure]] as a human-readable diagnostic string.
   *
   * Each case produces a distinct message that includes the relevant identifiers,
   * positions, or type information needed to understand and locate the error.
   *
   * @return a single-line diagnostic string describing the failure
   */
  def toPrettyString: String = failure match
    case ParseFailure(msg, pos) =>
      s"Parse error at $pos - $msg"
    case TypeFailure.Mismatch(got, expected @ _*) =>
      val expectedStr =
        if expected.isEmpty then "<unknown>"
        else expected.mkString(", ")
      s"Type mismatch: expected $expectedStr, got $got"
    case TypeFailure.UnknownVariable(name) =>
      s"Unknown variable: ${name.value}"
    case TypeFailure.UnknownType(name) =>
      s"Unknown type: ${name.value}"
    case TypeFailure.UnknownMember(className, memberName) =>
      s"Unknown member: $className#$memberName"
    case TypeFailure.VariableAlreadyDefined(name) =>
      s"Variable already defined: ${name.value}"
    case TypeFailure.TypeAlreadyDefined(name) =>
      s"Type already defined: ${name.value}"
    case TypeFailure.WrongArgumentCount(got, expected) =>
      s"Wrong number of argument: expected $expected, got $got"
    case TypeFailure.MissingTypeArguments(missing) =>
      s"Missing type arguments: ${missing.map(_.value).mkString(", ")}"
    case TypeFailure.TooManyTypeArguments(got, expected) =>
      s"Too many type arguments: expected ${expected.map(_.value).mkString(", ")}, got ${got.map(_.toString).mkString(", ")}"
    case TypeFailure.CannotInferType(name) =>
      s"Cannot infer type for variable: ${name.value}"
    case TypeFailure.ImmutableVariableAssignment(name) =>
      s"Cannot assign to immutable variable: ${name.value}"
    case TypeFailure.IllegalForwardReference(name) =>
      s"Illegal forward reference to variable: ${name.value}"
