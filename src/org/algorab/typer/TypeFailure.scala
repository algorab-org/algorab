package org.algorab.typer

import org.algorab.CompilerFailure
import org.algorab.ast.Type
import org.algorab.ast.Identifier
import kyo.Chunk

enum TypeFailure:
  case Mismatch(got: Type, expected: Type*)
  case UnknownVariable(name: Identifier)
  case UnknownType(name: Identifier)
  case VariableAlreadyDefined(name: Identifier)
  case TypeAlreadyDefined(name: Identifier)