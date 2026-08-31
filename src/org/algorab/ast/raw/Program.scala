package org.algorab.ast.raw

import io.github.iltotore.pureparser.Span
import org.algorab.ast.Identifier

/**
 * A parsed source file.
 *
 * @param packageName the package of this source file
 * @param statements the top-level statements
 */
case class Program(packageName: List[(Identifier, Span)], statements: List[Statement])
