package org.algorab.ast.resolved

import org.algorab.ast.SymbolId

enum Type derives CanEqual:
  case Ref(symbol: SymbolId)
  case Inferred
