package org.algorab.compilation

import org.algorab.ast.Instruction
import purelogic.*
import org.algorab.ast.InstructionPosition
import io.github.iltotore.iron.autoRefine
import io.github.iltotore.iron.assume

case class CompilationContext(instructions: Seq[Instruction], position: InstructionPosition)

object CompilationContext:

  def default(position: InstructionPosition): CompilationContext = CompilationContext(List.empty, position)

  def emit(instruction: Instruction): Compilation[Unit] = update(ctx => ctx.copy(
    instructions = ctx.instructions :+ instruction,
    position = ctx.position + 1
  ))

  def emitAll(instructions: Seq[Instruction]): Compilation[Unit] = update(ctx => ctx.copy(
    instructions = ctx.instructions ++ instructions,
    position = ctx.position + instructions.size.assume
  ))

  def currentPosition: Compilation[InstructionPosition] = get.position