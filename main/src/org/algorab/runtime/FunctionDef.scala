/** Runtime representation of a compiled function definition.
  *
  * A [[FunctionDef]] is created by [[VM]] when it encounters a
  * [[org.algorab.compiler.Instruction.FunctionStart]] instruction, and stored in the
  * [[RuntimeContext]] function table.  When [[org.algorab.compiler.Instruction.LoadFunction]]
  * is executed the VM uses [[captures]] to snapshot the relevant local variables into a
  * [[org.algorab.compiler.Value.UserDefinedFunction]] closure value.
  */
package org.algorab.runtime

import org.algorab.ast.Identifier
import org.algorab.compiler.InstrPosition

/** A runtime function descriptor.
  *
  * @param displayName the source-level function name, used for diagnostic output
  * @param captures    the set of local variable names that this function closes over;
  *                    these are captured by value when [[org.algorab.compiler.Instruction.LoadFunction]] runs
  * @param start       the absolute instruction position of the first instruction in the function body
  */
case class FunctionDef(displayName: Identifier, captures: Set[Identifier], start: InstrPosition)
