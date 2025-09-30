package org.algorab.ast

import kyo.Chunk

enum Type:
  case Inferred
  case Ref(name: Identifier)
  case Apply(base: Type, args: Chunk[Type])
  case Fun(params: Chunk[Type], output: Type)
  case Tuple(elements: Chunk[Type])