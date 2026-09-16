package org.algorab.ast.compiled

import org.algorab.ast.SymbolId

/**
  * A compiled module. All files under the same package represent one module.
  *
  * @param dependencies this module's dependencies
  * @param initialization this module's initialization function
  */
case class Module(
  dependencies: Set[SymbolId],
  initialization: SymbolId
)