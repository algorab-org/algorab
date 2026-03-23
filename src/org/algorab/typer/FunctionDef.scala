package org.algorab.typer

import kyo.Chunk
import org.algorab.ast.Identifier
import org.algorab.ast.tpd.Expr
import org.algorab.ast.tpd.Type

case class FunctionDef(displayName: Identifier, params: Chunk[Identifier], captures: Set[VariableId], body: Expr, varId: VariableId)
