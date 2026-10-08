package org.algorab.resolution

import org.algorab.AlgorabError
import org.algorab.ast.Identifier
import org.algorab.ast.Symbol
import org.algorab.ast.SymbolId
import org.algorab.show.Show
import org.algorab.util.SourcePosition

/**
 * An error occurring during the name resolution phase.
 */
enum ResolutionError extends AlgorabError.Frontend:

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
  case ForwardDeclaration(symbol: Symbol.Valid, position: SourcePosition)

  /**
   * The declared symbol has already been declared.
   *
   * @param symbol the symbol that was already declared under the same name
   * @param position the source position where the error occurred
   */
  case AlreadyDeclared(symbol: Symbol.Valid, position: SourcePosition)

  /**
   * The symbol was used as a namespace (e.g in a package declaration) while not being a namespace symbol.
   *
   * @param symbol the symbol unexpectedly used as a namespace
   * @param position the source position where the error occurred
   */
  case NotANamespace(symbol: Symbol, position: SourcePosition)

  /**
    * A non-definition statement is at the top-level of a non-script module, which is forbidden.
    *
    * @param position the source position where the error occurred
    */
  case TopLevelStatementInModule(position: SourcePosition)

  /**
    * A program can only have one script file, not multiple.
    *
    * @param position the source position where the error occurred
    */
  case MultipleScriptFiles(position: SourcePosition)

  override def message: Show[String] = this match
    case UnknownName(name, _)          => s"No variable, function or type named $name found. Is it imported?"
    case ForwardDeclaration(symbol, _) => s"${symbol.name} is used before its declaration. It is declared at ${symbol.position}."
    case AlreadyDeclared(symbol, _)    => s"${symbol.name} is already declared at ${symbol.position}."
    case NotANamespace(symbol, _)      => s"${symbol.name} cannot be used as a namespace."
    case TopLevelStatementInModule(_)  => s"Top-level statements are not allowed in non-script modules."
    case MultipleScriptFiles(_)        => s"You can only have one script file per project."
