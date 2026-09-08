package org.algorab.ast.typed

import org.algorab.ast.SymbolId

/**
 * A typed source file.
 *
 * @param owner the package of this source file
 * @param statements the top-level statements
 */
enum Program:
  case Script(statements: List[Statement])
  case Module(owner: SymbolId, definitions: List[Definition])