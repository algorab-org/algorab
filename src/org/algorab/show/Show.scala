package org.algorab.show

import purelogic.Reader
import org.algorab.ast.SymbolId
import org.algorab.ast.Symbol
import org.algorab.util.FileName
import org.algorab.parsing.FileInfo

/**
 * A program requiring printing context.
 */
type Show[+A] = Reader[ShowContext] ?=> A

object Show:

  /**
    * Evaluate the given [[Show]] program.
    *
    * @param context the printing context
    * @param program the program to evaluate
    * @return the result of the program
    */
  def apply[A](context: ShowContext)(program: Show[A]): A =
    Reader(context)(program)