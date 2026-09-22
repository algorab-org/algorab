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

  /**
   * The symbol id pointing to this program.
   */
  def symbol: SymbolId = this match
    case Script(_)        => SymbolId.Root
    case Module(owner, _) => owner

  /**
   * The statements of this program. A [[Program.Module]]'s statements are its definitions.
   */
  def moduleStatements: List[Statement] = this match
    case Script(statements)     => statements
    case Module(_, definitions) => definitions
