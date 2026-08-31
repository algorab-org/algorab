package org.algorab

import purelogic.Abort
import purelogic.Writer

/**
 * A program representation universal across all phases. Can produce 0 or more [[AlgorabError]] and stop.
 */
type AlgorabProgram[+A] = (Writer[AlgorabError], Abort[Unit]) ?=> A

object AlgorabProgram:

  /**
   * Run the given [[AlgorabProgram]].
   *
   * @param program the program to run
   * @return the program's result if it didn't abort, and the produced errors
   */
  def apply[A](program: AlgorabProgram[A]): (Seq[AlgorabError], Option[A]) = Writer(Abort(program).toOption)
