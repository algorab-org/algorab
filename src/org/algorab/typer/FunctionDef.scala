package org.algorab.typer

import org.algorab.ast.Identifier
import org.algorab.ast.tpd.Type
import org.algorab.ast.tpd.Expr

case class FunctionDef(displayName: Identifier, paramCount: Int, captures: Set[Identifier], body: Expr):

  def withCapture(capture: Identifier): FunctionDef =
    this.copy(captures = captures + capture)