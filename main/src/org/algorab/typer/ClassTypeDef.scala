package org.algorab.typer

import kyo.Chunk
import org.algorab.ast.Identifier
import org.algorab.ast.tpd.Expr

case class ClassTypeDef(
    displayName: Identifier,
    declarations: Map[Identifier, VariableId],
    parameters: Chunk[Identifier],
    captures: Set[VariableId],
    init: Chunk[Expr],
    varId: VariableId
)
