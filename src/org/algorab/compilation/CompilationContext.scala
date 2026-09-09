package org.algorab.compilation

import org.algorab.ast.compiled.Instruction
import org.algorab.ast.compiled.Function
import purelogic.*
import org.algorab.ast.InstructionPosition
import io.github.iltotore.iron.autoRefine
import io.github.iltotore.iron.assume
import org.algorab.ast.SymbolId

case class CompilationContext(
  functions: Map[SymbolId, Function],
  instructions: Seq[Instruction],
  position: InstructionPosition
)

object CompilationContext:

  val default: CompilationContext = CompilationContext(
    functions = Map.empty,
    instructions = Seq.empty,
    position = InstructionPosition(0)
  )

  def emit(instruction: Instruction): Compilation[Unit] = update(ctx => ctx.copy(
    instructions = ctx.instructions :+ instruction,
    position = ctx.position + 1
  ))

  def emitAll(instructions: Seq[Instruction]): Compilation[Unit] = update(ctx => ctx.copy(
    instructions = ctx.instructions ++ instructions,
    position = ctx.position + instructions.size.assume
  ))

  def currentPosition: Compilation[InstructionPosition] = get.position

  def addFunction(symbol: SymbolId, function: Function): Compilation[Unit] = update(ctx => ctx.copy(
    functions = ctx.functions.updated(symbol, function)
  ))