package org.algorab.typing

import org.algorab.AlgorabProgram
import org.algorab.ast.Symbol
import org.algorab.ast.SymbolId
import org.algorab.ast.resolved.Definition
import purelogic.*

/**
 * A program to be evaluated during the typing phase.
 */
type Typing[+A] = (State[TypeContext], Writer[TypeError]) ?=> A

object Typing:

  /**
   * Partially evaluate the given [[Typing]] program to an [[AlgorabProgram]].
   *
   * @param symbols the symbol table generated during the name resolution phase
   * @param declarations the declaration of each user-defined symbol generated during the name resolution phase
   * @param program the program to evaluate
   * @return an [[AlgorabProgram]] describing the same computation, with phase-specific effects evaluated
   */
  def apply[A](symbols: Map[SymbolId, Symbol], declarations: Map[SymbolId, Definition])(program: Typing[A]): AlgorabProgram[A] =
    State(TypeContext.default(symbols, declarations))(program)._2
