package org.algorab.ast.raw

import org.algorab.ast.Identifier
import org.algorab.util.SourcePosition

/**
 * A parsed source file.
 *
 * @param packageName the package of this source file
 * @param statements the top-level statements
 */
case class Program(packageName: List[(Identifier, SourcePosition)], statements: List[Statement])
