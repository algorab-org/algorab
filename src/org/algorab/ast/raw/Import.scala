package org.algorab.ast.raw

import org.algorab.ast.Identifier
import org.algorab.ast.raw.Import.Selector
import org.algorab.util.SourcePosition

/**
 * An import clause.
 *
 * @param path the path before the selector such as `a.b` in `import a.b.c`
 * @param selector the member selector
 * @param position the source position of this import
 */
case class Import(path: List[(Identifier, SourcePosition)], selector: Selector, position: SourcePosition)

object Import:

  /**
   * A member selector for an import.
   */
  enum Selector:

    /**
     * Import a named member.
     *
     * @param name the name of the member to import
     * @param position the source position of this selector
     */
    case Simple(name: Identifier, position: SourcePosition)

    /**
     * Import all members.
     *
     * @param position the source position of this selector
     */
    case Wildcard(position: SourcePosition)

    /**
     * Import a named member under another name.
     *
     * @param name the name of the member to import
     * @param alias the name the member is imported as
     * @param position the source position of this selector
     */
    case Rename(name: Identifier, alias: Identifier, position: SourcePosition)

    /**
     * The source position of this selector.
     */
    def position: SourcePosition
