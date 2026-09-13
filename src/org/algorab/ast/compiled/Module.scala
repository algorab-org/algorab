package org.algorab.ast.compiled

import org.algorab.ast.SymbolId

case class Module(
  dependencies: Set[SymbolId],
  initialization: SymbolId
)