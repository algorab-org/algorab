package org.algorab.typing

import org.algorab.ast.typed.Type as TypeValue

enum TypePattern:
  case Type(tpe: TypeValue)
  case Union(patterns: List[TypePattern])
  case Operator(left: TypePattern, right: TypePattern, text: String)