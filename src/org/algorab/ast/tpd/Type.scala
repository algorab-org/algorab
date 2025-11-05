package org.algorab.ast.tpd

import kyo.Chunk
import org.algorab.ast.Identifier

enum Type derives CanEqual:
  case Inferred
  case Generic(name: Identifier)
  case Class(name: Identifier)
  case Apply(base: Type, args: Chunk[Type])
  case Fun(params: Chunk[Type], output: Type)
  case TypeFun(typeParams: Chunk[Identifier], output: Type)
  case Tuple(elements: Chunk[Type])

  def zip(other: Type): Type = Type.Tuple(Chunk(this, other))

  def notInferredOr(other: Type): Type =
    if this == Inferred then other
    else this

object Type:

  // Std types
  val Any: Type = Class(Identifier("Any"))
  val Unit: Type = Class(Identifier("Unit"))
  val Boolean: Type = Class(Identifier("Boolean"))
  val Int: Type = Class(Identifier("Int"))
  val Float: Type = Class(Identifier("Float"))
  val Char: Type = Class(Identifier("Char"))
  val String: Type = Class(Identifier("String"))
  val Array: Type = Class(Identifier("Array"))
