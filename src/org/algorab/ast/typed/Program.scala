package org.algorab.ast.typed

import org.algorab.ast.SymbolId

case class Program(owner: SymbolId, statements: List[Statement])