package org.algorab.typing

import org.algorab.ast.AbsoluteId
import org.algorab.ast.tpd.Type
import org.algorab.ast.Identifier

case class TypeContext(scopes: List[TypeScope], definitions: Map[AbsoluteId, Type])

object TypeContext:

  val default: TypeContext = TypeContext(
    scopes = List(
      TypeScope(Map.empty),
      Map.empty
    )
  )

  def getReference(name: Identifier): Typing[]