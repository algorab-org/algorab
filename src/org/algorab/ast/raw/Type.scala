package org.algorab.ast.raw

import org.algorab.ast.Identifier

enum Type derives CanEqual:
  case Ref(name: Identifier)
  case Inferred
