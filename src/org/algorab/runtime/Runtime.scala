package org.algorab.runtime

import purelogic.*
import org.algorab.AlgorabProgram
import org.algorab.ast.compiled.Program

type Runtime[+A] = (State[RuntimeContext], Abort[RuntimeError]) ?=> A

object Runtime:

  def apply[A](compiledProgram: Program)(program: Runtime[A]): AlgorabProgram[A] =
    State(RuntimeContext.default(compiledProgram.modules, compiledProgram.functions))(
      Abort.recover(program): error =>
        write(error)
        fail(())
    )._2