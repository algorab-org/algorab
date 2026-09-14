package org.algorab.runtime

import org.algorab.ast.SymbolId
import org.algorab.ast.Value
import org.algorab.ast.InstructionPosition

case class RuntimeFrame(
  currentFunction: SymbolId,
  position: InstructionPosition,
  variables: Map[SymbolId, Value],
  stack: List[Value]
)

object RuntimeFrame:

  def default(currentFunction: SymbolId, stack: List[Value]): RuntimeFrame = RuntimeFrame(
    currentFunction = currentFunction,
    position = InstructionPosition(0),
    variables = Map.empty,
    stack = stack
  )