package org.algorab.typer

import kyo.Chunk
import org.algorab.CompilerFailure
import org.algorab.ast.Identifier
import org.algorab.ast.tpd
import org.algorab.ast.untpd

enum TypeFailure:
  case Mismatch(got: tpd.Type, expected: tpd.Type*)
  case UnknownVariable(name: Identifier)
  case UnknownType(name: Identifier)
  case UnknownMember(className: Identifier, fieldName: Identifier)
  case VariableAlreadyDefined(name: Identifier)
  case TypeAlreadyDefined(name: Identifier)
  case WrongArgumentCount(got: Int, size: Int)
  case MissingTypeArguments(missing: Chunk[Identifier])
  case TooManyTypeArguments(got: Chunk[untpd.Type], expected: Chunk[Identifier])
  case CannotInferType(name: Identifier)
  case ImmutableVariableAssignment(name: Identifier)
  case IllegalForwardReference(name: Identifier)