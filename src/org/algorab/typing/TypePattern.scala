package org.algorab.typing

import org.algorab.ast.typed.Type as TypeValue

/**
 * A type pattern, usually used to express expected types.
 */
enum TypePattern:

  /**
   * A pattern representing the given type.
   *
   * @param tpe the represented type
   */
  case Type(tpe: TypeValue)

  /**
   * An union between different patterns.
   *
   * @param patterns the allowed patterns
   */
  case Union(patterns: List[TypePattern])

  /**
   * A pattern to express binary operators.
   *
   * @param left the expected pattern at the left of the operator
   * @param right the expected pattern at the right of the operator
   * @param text the textual representation of the operator such as "+" or "and"
   */
  case BinaryOperator(left: TypePattern, right: TypePattern, text: String)
