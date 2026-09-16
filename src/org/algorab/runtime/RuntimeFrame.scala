package org.algorab.runtime

import org.algorab.ast.InstructionPosition
import org.algorab.ast.SymbolId
import org.algorab.ast.Value

/**
 * A runtime call frame containing the execution state of a function.
 *
 * @param currentFunction the function being executed
 * @param position the position of the next instruction
 * @param variables the local variables
 * @param stack the current value stack
 */
case class RuntimeFrame(
    currentFunction: SymbolId,
    position: InstructionPosition,
    variables: Map[SymbolId, Value],
    stack: List[Value]
)

object RuntimeFrame:

  /**
   * Create a default runtime frame for a function.
   *
   * @param currentFunction the function to execute
   * @param stack the initial stack
   * @return the initialized runtime frame
   */
  def default(currentFunction: SymbolId, stack: List[Value]): RuntimeFrame = RuntimeFrame(
    currentFunction = currentFunction,
    position = InstructionPosition(0),
    variables = Map.empty,
    stack = stack
  )
