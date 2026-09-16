package org.algorab.compilation

import org.algorab.AlgorabProgram
import org.algorab.ast.InstructionPosition
import org.algorab.ast.SymbolId
import org.algorab.ast.compiled.Instruction
import purelogic.*

/**
 * A program to be evaluated during the compilation phase.
 */
type Compilation[+A] = State[CompilationContext] ?=> A

object Compilation:

  /**
   * Partially evaluate the given [[Compilation]] program to an [[AlgorabProgram]].
   *
   * @param program the program to evaluate
   * @return an [[AlgorabProgram]] describing the same computation, with phase-specific effects evaluated
   */
  def apply[A](program: Compilation[A]): AlgorabProgram[(CompilationContext, A)] =
    State(CompilationContext.default)(program)

  /**
   * Locally evaluate the given [[Compilation]] program.
   * The new compiled function bodies and globals are reflected to this [[CompilationContext]].
   *
   * @param program the program to evaluate in a local compilation context containing the same declared functions and globals.
   * @return the compiled instructions
   */
  def locally(program: Compilation[Unit]): Compilation[Seq[Instruction]] =
    locallyAt(InstructionPosition(0))(program)._1

  /**
   * Locally evaluate the given [[Compilation]] program, offsetting the position.
   * This is useful to make emitted [[Instruction.Jump]]/[[Instruction.JumpIfFalse]] point to the right position.
   * The new compiled function bodies and globals are reflected to this [[CompilationContext]].
   *
   * @param offset the offset used for instruction positions
   * @param program the program to evaluate in a local compilation context containing the same declared functions and globals.
   * @return the compiled instructions
   */
  def locallyAt(offset: InstructionPosition)(program: Compilation[Unit]): Compilation[(Seq[Instruction], InstructionPosition)] =
    val context = localState(_.copy(instructions = Vector.empty, position = offset)):
      program
      get

    update(ctx => ctx.copy(functions = context.functions, globals = context.globals))
    (context.instructions, context.position)
