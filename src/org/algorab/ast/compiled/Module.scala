package org.algorab.ast.compiled

import org.algorab.ast.SymbolId

/**
 * A compiled module. All files under the same package represent one module.
 *
 * @param initialization this module's initialization function
 */
case class Module(
    initialization: SymbolId
)
