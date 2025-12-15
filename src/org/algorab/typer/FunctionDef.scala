package org.algorab.typer

import org.algorab.ast.Identifier
import org.algorab.ast.tpd.Type
import org.algorab.ast.tpd.Expr
import kyo.Chunk

case class FunctionDef(displayName: Identifier, params: Chunk[Identifier], captures: Set[VariableId], body: Expr)