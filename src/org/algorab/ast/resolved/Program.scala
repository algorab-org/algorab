package org.algorab.ast.resolved

import org.algorab.ast.SymbolId

case class Program(owner: SymbolId, statements: List[Statement])