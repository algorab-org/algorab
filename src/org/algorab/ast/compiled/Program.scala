package org.algorab.ast.compiled

import org.algorab.ast.SymbolId

case class Program(
  modules: Map[SymbolId, Module],
  functions: Map[SymbolId, Function]
)