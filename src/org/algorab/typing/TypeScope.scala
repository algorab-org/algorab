package org.algorab.typing

import org.algorab.ast.Identifier
import org.algorab.ast.untpd.Type
import org.algorab.ast.AbsoluteId

case class TypeScope(references: Map[Identifier, AbsoluteId]):

  def withReference(name: Identifier, absolute: AbsoluteId): TypeScope =
    this.copy(references = references.updated(name, absolute))