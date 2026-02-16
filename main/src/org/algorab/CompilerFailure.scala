package org.algorab

import org.algorab.typer.TypeFailure
import kyo.ParseFailure

type CompilerFailure = ParseFailure | TypeFailure

extension (failure: CompilerFailure)
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
    case TypeFailure.VariableAlreadyDefined(name) =>
      s"Variable already defined: ${name.value}"
    case TypeFailure.TypeAlreadyDefined(name) =>
      s"Type already defined: ${name.value}"
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