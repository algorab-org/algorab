package org.algorab.ast.raw

import org.algorab.ast.Identifier

/**
 * A parsed type.
 */
enum Type derives CanEqual:

  /**
   * A reference to a declared definition.
   *
   * @param name the name of the definition
   */
  case Ref(name: Identifier)

  /**
   * An inferred type aka not explicitly typed in the source.
   */
  case Inferred
