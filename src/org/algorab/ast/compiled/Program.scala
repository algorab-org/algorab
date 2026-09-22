package org.algorab.ast.compiled

import org.algorab.ast.SymbolId

/**
 * A compiled program.
 *
 * @param modules the compiled modules
 * @param functions the compiled functions
 * @param owners the module owner of each global symbol
 */
case class Program(
    modules: Map[SymbolId, Module],
    functions: Map[SymbolId, Function],
    owners: Map[SymbolId, SymbolId]
)
