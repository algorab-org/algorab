package org.algorab.ast

enum Type derives CanEqual:
  case Ref(name: Identifier)
  case Inferred