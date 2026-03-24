package org.algorab.ast.tpd

import kyo.Chunk
import org.algorab.ast.Identifier

enum Type derives CanEqual:
  case Nothing
  case Inferred
  case Generic(name: Identifier)
  case Class(name: Identifier)
  case Instance(name: Identifier)
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
    case Instance(name)              => Type.Instance(name)
    case Apply(base, args)           => Type.Apply(base.replaceGeneric(replacements), args.map(_.replaceGeneric(replacements)))
    case Fun(params, output)         => Type.Fun(params.map(_.replaceGeneric(replacements)), output.replaceGeneric(replacements))
    case TypeFun(typeParams, output) => Type.TypeFun(typeParams, output.replaceGeneric(replacements))
    case Tuple(elements)             => Type.Tuple(elements.map(_.replaceGeneric(replacements)))

object Type:

  // Std types
  val Any: Type = Instance(Identifier("Any"))
  val Unit: Type = Instance(Identifier("Unit"))
  val Boolean: Type = Instance(Identifier("Boolean"))
  val Int: Type = Instance(Identifier("Int"))
  val Float: Type = Instance(Identifier("Float"))
  val Char: Type = Instance(Identifier("Char"))
  val String: Type = Instance(Identifier("String"))
  val Array: Type = Instance(Identifier("Array"))
  def arrayOf(tpe: Type): Type = Type.Apply(Array, Chunk(tpe))
