package org.algorab.ast.tpd

import java.util.Objects
import kyo.Chunk
import org.algorab.ast.Identifier

enum Type derives CanEqual:
  case Nothing
  case Inferred
  case Generic(name: Identifier)
  case Class(name: Identifier, constructor: Type)
  case Instance(name: Identifier, typeParams: Chunk[Identifier], replacements: Map[Identifier, Type])
  case Apply(base: Type, args: Chunk[Type])
  case Fun(params: Chunk[Type], output: Type)
  case TypeFun(typeParams: Chunk[Identifier], output: Type)
  case Tuple(elements: Chunk[Type])

  def zip(other: Type): Type = Type.Tuple(Chunk(this, other))

  def notInferredOr(other: Type): Type =
    if this == Inferred then other
    else this

  def replaceGeneric(replacements: Map[Identifier, Type]): Type = this match
    case Nothing                             => Type.Nothing
    case Inferred                            => Type.Inferred
    case Generic(n)                          => replacements.getOrElse(n, this)
    case Class(name, constructor)            => Type.Class(name, constructor.replaceGeneric(replacements))
    case Instance(name, typeParams, members) => Type.Instance(name, typeParams, members.map((k, v) => (k, v.replaceGeneric(replacements))))
    case Apply(base, args)                   => Type.Apply(base.replaceGeneric(replacements), args.map(_.replaceGeneric(replacements)))
    case Fun(params, output)                 => Type.Fun(params.map(_.replaceGeneric(replacements)), output.replaceGeneric(replacements))
    case TypeFun(typeParams, output)         => Type.TypeFun(typeParams, output.replaceGeneric(replacements))
    case Tuple(elements)                     => Type.Tuple(elements.map(_.replaceGeneric(replacements)))

object Type:

  // Std types
  val Any: Type = Instance(Identifier("Any"), Chunk.empty, Map.empty)
  val Unit: Type = Instance(Identifier("Unit"), Chunk.empty, Map.empty)
  val Boolean: Type = Instance(Identifier("Boolean"), Chunk.empty, Map.empty)
  val Int: Type = Instance(Identifier("Int"), Chunk.empty, Map.empty)
  val Float: Type = Instance(Identifier("Float"), Chunk.empty, Map.empty)
  val Char: Type = Instance(Identifier("Char"), Chunk.empty, Map.empty)
  val String: Type = Instance(Identifier("String"), Chunk.empty, Map.empty)
  val Array: Type = Instance(Identifier("Array"), Chunk.empty, Map.empty)
  def arrayOf(tpe: Type): Type = Type.Apply(Array, Chunk(tpe))
