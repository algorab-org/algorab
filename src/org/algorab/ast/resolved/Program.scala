package org.algorab.ast.resolved

import org.algorab.ast.SymbolId

/**
 * A resolved source file.
 *
 * @param owner the package of this source file
 * @param statements the top-level statements
 */
enum Program:
  case Script(statements: List[Statement])
  case Module(owner: SymbolId, definitions: List[Definition])