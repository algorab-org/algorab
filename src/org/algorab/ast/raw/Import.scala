package org.algorab.ast.raw

import io.github.iltotore.pureparser.Span
import org.algorab.ast.Identifier
import org.algorab.ast.raw.Import.Selector

/**
 * An import clause.
 *
 * @param path the path before the selector such as `a.b` in `import a.b.c`
 * @param selector the member selector
 * @param span the source position of this import
 */
case class Import(path: List[(Identifier, Span)], selector: Selector, span: Span)

object Import:

  /**
   * A member selector for an import.
   */
  enum Selector:

    /**
     * Import a named member.
     *
     * @param name the name of the member to import
     * @param span the source position of this selector
     */
    case Simple(name: Identifier, span: Span)

    /**
     * Import all members.
     *
     * @param span the source position of this selector
     */
    case Wildcard(span: Span)

    /**
     * Import a named member under another name.
     *
     * @param name the name of the member to import
     * @param alias the name the member is imported as
     * @param span the source position of this selector
     */
    case Rename(name: Identifier, alias: Identifier, span: Span)

    /**
     * The source position of this selector.
     */
    def span: Span
