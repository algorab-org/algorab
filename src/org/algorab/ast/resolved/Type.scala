package org.algorab.ast.resolved

import org.algorab.ast.SymbolId

/**
 * A parsed type.
 */
enum Type derives CanEqual:

  /**
   * A reference to a declared definition.
   *
   * @param symbol the unique id of the referenced definition
   */
  case Ref(symbol: SymbolId)

  /**
   * An inferred type aka not explicitly typed in the source.
   */
  case Inferred
