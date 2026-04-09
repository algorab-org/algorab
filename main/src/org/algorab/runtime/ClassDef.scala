/** Runtime representation of a compiled class definition.
  *
  * A [[ClassDef]] is created by [[VM]] when it encounters a
  * [[org.algorab.compiler.Instruction.ClassStart]] instruction, and stored in the
  * [[RuntimeContext]] class table.  When the class is later instantiated (via
  * [[org.algorab.compiler.Instruction.Apply]] on a [[org.algorab.compiler.Value.VClass]]),
  * the VM jumps to [[initStart]] to run the constructor body.
  */
package org.algorab.runtime

import org.algorab.ast.Identifier
import org.algorab.compiler.InstrPosition

/** A runtime class descriptor.
  *
  * @param displayName the source-level class name, used for diagnostic output
  * @param initStart   the absolute instruction position of the first constructor instruction
  */
case class ClassDef(displayName: Identifier, initStart: InstrPosition)
