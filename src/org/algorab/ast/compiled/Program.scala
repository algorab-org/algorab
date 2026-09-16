package org.algorab.ast.compiled

import org.algorab.ast.SymbolId

/**
  * A compiled program.
  *
  * @param modules the compiled modules
  * @param functions the compiled functions
  */
case class Program(
  modules: Map[SymbolId, Module],
  functions: Map[SymbolId, Function]
)