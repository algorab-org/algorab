package org.algorab.resolution

import org.algorab.AlgorabProgram
import purelogic.*

/**
 * A program to be evaluated during the name resolution phase.
 */
type Resolution[+A] = (State[ResolutionContext], Writer[ResolutionError]) ?=> A

object Resolution:

  /**
   * Partially evaluate the given [[Resolution]] program to an [[AlgorabProgram]].
   *
   * @param program the program to evaluate
   * @return an [[AlgorabProgram]] describing the same computation, with phase-specific effects evaluated
   */
  def apply[A](program: Resolution[A]): AlgorabProgram[(ResolutionContext, A)] =
    State(ResolutionContext.default)(program)
