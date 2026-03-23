package org.algorab.runtime

import org.algorab.ast.Identifier
import org.algorab.compiler.InstrPosition

case class FunctionDef(displayName: Identifier, captures: Set[Identifier], start: InstrPosition)
