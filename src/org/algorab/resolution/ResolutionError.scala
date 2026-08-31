package org.algorab.resolution

import io.github.iltotore.pureparser.Span
import org.algorab.ast.Identifier
import org.algorab.ast.Symbol

/**
 * An error occurring during the name resolution phase.
 */
enum ResolutionError:

  /**
   * The referenced name does not exist.
   *
   * @param name the queried name
   * @param span the source position where the error occurred
   */
  case UnknownName(name: Identifier, span: Span)

  /**
   * The referenced symbol is used before its declaration/having been initialized.
   * This usually occurs for variables being used before their declaration.
   *
   * @param symbol the symbol used before its declaration
   * @param span the source position where the error occurred
   */
  case ForwardDeclaration(symbol: Symbol, span: Span)

  /**
   * The declared symbol has already been declared.
   *
   * @param symbol the symbol that was already declared under the same name
   * @param span the source position where the error occurred
   */
  case AlreadyDeclared(symbol: Symbol, span: Span)

  /**
   * The symbol was used as a namespace (e.g in a package declaration) while not being a namespace symbol.
   *
   * @param symbol the symbol unexpectedly used as a namespace
   * @param span the source position where the error occurred
   */
  case NotANamespace(symbol: Symbol, span: Span)

  /**
   * The source position where the error occurred.
   */
  def span: Span
