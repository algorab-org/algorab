package org.algorab.typer

import org.algorab.CompilerFailure
import org.algorab.ast.untpd
import org.algorab.ast.tpd
import org.algorab.ast.Identifier
import kyo.Chunk

enum TypeFailure:
  case Mismatch(got: tpd.Type, expected: tpd.Type*)
  case UnknownVariable(name: Identifier)
  case UnknownType(name: Identifier)
  case VariableAlreadyDefined(name: Identifier)
  case TypeAlreadyDefined(name: Identifier)
  case MissingTypeArguments(missing: Chunk[Identifier])
  case TooManyTypeArguments(got: Chunk[untpd.Type], expected: Chunk[Identifier])
  case CannotInferType(name: Identifier)