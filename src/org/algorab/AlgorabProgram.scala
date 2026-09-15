package org.algorab

import purelogic.Abort
import purelogic.Writer
import org.algorab.util.Console

/**
 * A program representation universal across all phases. Can produce 0 or more [[AlgorabError]] and stop.
 */
type AlgorabProgram[+A] = (Writer[AlgorabError], Abort[Unit], Console) ?=> A

object AlgorabProgram:

  /**
   * Run the given [[AlgorabProgram]].
   *
   * @param program the program to run
   * @return the program's result if it didn't abort, and the produced errors
   */
  def apply[A](program: AlgorabProgram[A]): (Seq[AlgorabError], Option[A]) = Console.withStd(Writer(Abort(program).toOption))

  def withInput[A](input: String)(program: AlgorabProgram[A]): (String, Seq[AlgorabError], Option[A]) =
    val (output, result) = Console.withInput(input)(Writer(Abort(program).toOption))
    output *: result