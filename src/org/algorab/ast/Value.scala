package org.algorab.ast

import org.algorab.ast.typed.Type
import org.algorab.runtime.Runtime

opaque type Value <: Value.Raw = Value.Raw
object Value:
  type Raw = Boolean | Int | Double | Char | String | Unit | Array[Any] | FunctionRef | BuiltinFunction | Null

  given CanEqual[Value, Value] = CanEqual.derived

  inline def apply(value: Raw): Value = value

  inline def unapply(value: Raw): Value = value

  def default(tpe: Type): Value = tpe match
    case Type.Boolean => false
    case Type.Int     => 0
    case Type.Float   => 0.0
    case Type.Char    => '\u0000'
    case _            => null

  def toString(value: Value): String = value match
    case FunctionRef(id) => s"<function $id>"
    case BuiltinFunction(f) => "<builtin function>"
    case _: Unit => "Unit"
    case array: Array[Any] => array.map(value => Value.toString(value.asInstanceOf[Value])).mkString("Array(", ", ", ")")
    case _ => value.toString()
  

  case class FunctionRef(id: SymbolId)

  object FunctionRef:
    def apply(id: SymbolId): Value = new FunctionRef(id)

  case class BuiltinFunction(f: Seq[Value] => Runtime[Value])

  object BuiltinFunction:
    def apply(f: Seq[Value] => Runtime[Value]): Value = new BuiltinFunction(f)
