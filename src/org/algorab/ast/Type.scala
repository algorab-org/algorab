package org.algorab.ast

enum Type:
  case Inferred
  case Ref(name: Identifier)