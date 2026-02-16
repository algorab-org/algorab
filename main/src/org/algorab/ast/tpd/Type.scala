package org.algorab.ast.tpd

import kyo.Chunk
import org.algorab.ast.Identifier

enum Type derives CanEqual:
  case Nothing
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

  def replaceGeneric(replacements: Map[Identifier, Type]): Type = this match
    case Nothing                     => Type.Nothing
    case Inferred                    => Type.Inferred
    case Generic(n)                  => replacements.getOrElse(n, this)
    case Class(name)                 => Type.Class(name)
    case Apply(base, args)           => Type.Apply(base.replaceGeneric(replacements), args.map(_.replaceGeneric(replacements)))
    case Fun(params, output)         => Type.Fun(params.map(_.replaceGeneric(replacements)), output.replaceGeneric(replacements))
    case TypeFun(typeParams, output) => Type.TypeFun(typeParams, output.replaceGeneric(replacements))
    case Tuple(elements)             => Type.Tuple(elements.map(_.replaceGeneric(replacements)))

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
  def arrayOf(tpe: Type): Type = Type.Apply(Array, Chunk(tpe))
