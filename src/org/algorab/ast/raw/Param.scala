package org.algorab.ast.raw

import org.algorab.ast.Identifier
import org.algorab.util.SourcePosition

enum Param:
  case Simple(name: Identifier, tpe: Type, default: Option[Expr], position: SourcePosition)
  case Varargs(name: Identifier, tpe: Type, position: SourcePosition)

  def name: Identifier