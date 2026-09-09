package org.algorab.compilation

import purelogic.*
import org.algorab.AlgorabProgram
import org.algorab.ast.InstructionPosition
import org.algorab.ast.SymbolId
import org.algorab.ast.compiled.Instruction

type Compilation[+A] = State[CompilationContext] ?=> A

object Compilation:

  def apply[A](program: Compilation[A]): AlgorabProgram[(CompilationContext, A)] =
    State(CompilationContext.default)(program)

  def locally(program: Compilation[Unit]): Compilation[Seq[Instruction]] =
    locallyAt(InstructionPosition(0))(program)._1

  def locallyAt(offset: InstructionPosition)(program: Compilation[Unit]): Compilation[(Seq[Instruction], InstructionPosition)] =
    val context = localState(_.copy(position = offset)):
      program
      get

    update(ctx => ctx.copy(functions = context.functions))
    (context.instructions, context.position)