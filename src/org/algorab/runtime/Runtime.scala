package org.algorab.runtime

import purelogic.*
import org.algorab.AlgorabProgram
import org.algorab.ast.compiled.Program
import org.algorab.util.Console
import org.algorab.util.ConsoleError

type Runtime[+A] = (State[RuntimeContext], Abort[RuntimeError], Console) ?=> A

object Runtime:

  def apply[A](compiledProgram: Program)(program: Runtime[A]): AlgorabProgram[A] =
    State(RuntimeContext.default(compiledProgram.modules, compiledProgram.functions))(
      Abort.recover(program): error =>
        write(error)
        fail(())
    )._2

  def convertConsoleError[A](program: (State[RuntimeContext], Abort[ConsoleError]) ?=> A): Runtime[A] =
    Abort.recover(program)(error => fail(RuntimeError.Console(error, RuntimeContext.currentSpan)))
