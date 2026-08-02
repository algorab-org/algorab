package org.algorab.ast.raw

import org.algorab.ast.Identifier
import io.github.iltotore.pureparser.Span

case class Program(packageName: List[(Identifier, Span)], statements: List[Statement])