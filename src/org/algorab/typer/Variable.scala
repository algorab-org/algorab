package org.algorab.typer

import kyo.Absent
import kyo.Maybe
import org.algorab.ast.Identifier
import org.algorab.ast.tpd.Type

case class Variable(
  localName: Identifier,
  tpe: Type,
  mutable: Boolean,
  boxxed: Boolean,
  initialized: Boolean,
  functionId: Maybe[Identifier] = Absent
)
