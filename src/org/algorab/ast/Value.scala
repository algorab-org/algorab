package org.algorab.ast

import org.algorab.ast.typed.Type

opaque type Value <: Value.Raw = Value.Raw
object Value:
  type Raw = Boolean | Int | Double | Char | String | Null

  inline def apply(value: Raw): Value = value

  inline def unapply(value: Raw): Value = value

  def default(tpe: Type): Value = tpe match
    case Type.Boolean => false
    case Type.Int     => 0
    case Type.Float   => 0.0
    case Type.Char    => '\u0000'
    case _            => null
