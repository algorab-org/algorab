package org.algorab.typing

import purelogic.*
import org.algorab.AlgorabProgram
import org.algorab.ast.SymbolId
import org.algorab.ast.Symbol
import org.algorab.ast.resolved.Definition

type Typing[+A] = (State[TypeContext], Writer[TypeError])?=> A

object Typing:

  def apply[A](symbols: Map[SymbolId, Symbol], declarations: Map[SymbolId, Definition])(program: Typing[A]): AlgorabProgram[A] =
    State(TypeContext.default(symbols, declarations))(program)._2