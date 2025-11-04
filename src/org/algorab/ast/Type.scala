package org.algorab.ast

import kyo.Chunk

enum Type derives CanEqual:
  case Inferred
  case Ref(name: Identifier)
  case Apply(base: Type, args: Chunk[Type])
  case Fun(params: Chunk[Type], output: Type)
  case Tuple(elements: Chunk[Type])

  def zip(other: Type): Type = Type.Tuple(Chunk(this, other))

  def notInferredOr(other: Type): Type =
    if this == Inferred then other
    else this

object Type:

  // Std types
  val Any: Type = Ref(Identifier("Any"))
  val Unit: Type = Ref(Identifier("Unit"))
  val Boolean: Type = Ref(Identifier("Boolean"))
  val Int: Type = Ref(Identifier("Int"))
  val Float: Type = Ref(Identifier("Float"))
  val Char: Type = Ref(Identifier("Char"))
  val String: Type = Ref(Identifier("String"))
  val Array: Type = Ref(Identifier("Array"))
