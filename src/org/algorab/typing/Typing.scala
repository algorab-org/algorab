package org.algorab.typing

import purelogic.*
import org.algorab.AlgorabProgram
import org.algorab.ast.SymbolId
import org.algorab.ast.Symbol

type Typing[+A] = (State[TypeContext], Writer[TypeError])?=> A

object Typing:

  def apply[A](symbols: Map[SymbolId, Symbol])(program: Typing[A]): AlgorabProgram[A] =
    State(TypeContext.empty(symbols))(program)._2