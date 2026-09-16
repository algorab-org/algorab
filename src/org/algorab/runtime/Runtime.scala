package org.algorab.runtime

import purelogic.*
import org.algorab.AlgorabProgram
import org.algorab.ast.compiled.Program
import org.algorab.util.Console
import org.algorab.util.ConsoleError

/**
  * A program to be evaluated at runtime.
  */
type Runtime[+A] = (State[RuntimeContext], Abort[RuntimeError], Console) ?=> A

object Runtime:

  /**
    * Partially evaluate the given [[Runtime]] program to an [[AlgorabProgram]].
    *
    * @param compiledProgram the compiled program containing modules and bodies
    * @param program the program to evaluate
    * @return an [[AlgorabProgram]] describing the same computation, with phase-specific effects evaluated
    */
  def apply[A](compiledProgram: Program)(program: Runtime[A]): AlgorabProgram[A] =
    State(RuntimeContext.default(compiledProgram.modules, compiledProgram.functions))(
      Abort.recover(program): error =>
        write(error)
        fail(())
    )._2

  /**
    * Partially evaluate the given [[Console]] program to a [[Runtime]] program.
    * This basically converts the thrown [[ConsoleError]], if any, to a [[RuntimeError]].
    *
    * @param program the program to evaluate
    * @return a [[Runtime]] describing the same computation
    */
  def convertConsoleError[A](program: (State[RuntimeContext], Abort[ConsoleError]) ?=> A): Runtime[A] =
    Abort.recover(program)(error => fail(RuntimeError.Console(error, RuntimeContext.currentSpan)))
