package org.algorab.resolution

import org.algorab.ast.Identifier
import org.algorab.ast.Symbol
import org.algorab.util.SourcePosition

/**
 * An error occurring during the name resolution phase.
 */
enum ResolutionError:

  /**
   * The referenced name does not exist.
   *
   * @param name the queried name
   * @param position the source position where the error occurred
   */
  case UnknownName(name: Identifier, position: SourcePosition)

  /**
   * The referenced symbol is used before its declaration/having been initialized.
   * This usually occurs for variables being used before their declaration.
   *
   * @param symbol the symbol used before its declaration
   * @param position the source position where the error occurred
   */
  case ForwardDeclaration(symbol: Symbol, position: SourcePosition)

  /**
   * The declared symbol has already been declared.
   *
   * @param symbol the symbol that was already declared under the same name
   * @param position the source position where the error occurred
   */
  case AlreadyDeclared(symbol: Symbol, position: SourcePosition)

  /**
   * The symbol was used as a namespace (e.g in a package declaration) while not being a namespace symbol.
   *
   * @param symbol the symbol unexpectedly used as a namespace
   * @param position the source position where the error occurred
   */
  case NotANamespace(symbol: Symbol, position: SourcePosition)

  case TopLevelStatementInModule(position: SourcePosition)

  case MultipleScriptFiles(position: SourcePosition) // TODO use SourcePosition

  /**
   * The source position where the error occurred.
   */
  def position: SourcePosition
