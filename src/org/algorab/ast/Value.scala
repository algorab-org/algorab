package org.algorab.ast

import org.algorab.ast.typed.Type
import org.algorab.runtime.Runtime

opaque type Value <: Value.Raw = Value.Raw
object Value:
  type Raw = Boolean | Int | Double | Char | String | Unit | FunctionRef | BuiltinFunction | Null

  given CanEqual[Value, Value] = CanEqual.derived

  inline def apply(value: Raw): Value = value

  inline def unapply(value: Raw): Value = value

  def default(tpe: Type): Value = tpe match
    case Type.Boolean => false
    case Type.Int     => 0
    case Type.Float   => 0.0
    case Type.Char    => '\u0000'
    case _            => null

  case class FunctionRef(id: SymbolId)

  object FunctionRef:
    def apply(id: SymbolId): Value = new FunctionRef(id)

  case class BuiltinFunction(f: Seq[Value] => Runtime[Value])

  object BuiltinFunction:
    def apply(f: Seq[Value] => Runtime[Value]): Value = new BuiltinFunction(f)
