package org.algorab.ast.raw

import org.algorab.ast.Identifier

case class Program(packageName: List[Identifier], statements: List[Statement])