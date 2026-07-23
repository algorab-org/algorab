package org.algorab.ast.untpd

import org.algorab.ast.Identifier

enum Type derives CanEqual:
  case Ref(name: Identifier)
  case Inferred
  case Invalid