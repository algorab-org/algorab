package org.algorab.compilation

import purelogic.*
import org.algorab.AlgorabProgram
import org.algorab.ast.InstructionPosition

type Compilation[+A] = State[CompilationContext] ?=> A

object Compilation:

  def apply[A](program: Compilation[A]): AlgorabProgram[A] =
    State(CompilationContext.default(InstructionPosition(0)))(program)._2

  def locally[A](offset: InstructionPosition)(program: Compilation[Unit]): CompilationContext =
    State(CompilationContext.default(offset))(program)._1