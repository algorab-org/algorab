package org.algorab.compiler

import kyo.*

enum Value derives CanEqual:
  case VUnit
  case VBool(value: Boolean)
  case VInt(value: Int)
  case VFloat(value: Double)
  case VChar(value: Char)
  case VString(value: String)
  case UserDefinedFunction(position: InstrPosition)
  case BuiltInFunction(f: Chunk[Value] => Value < Sync)

  def asBool: Boolean = this match
    case VBool(value) => value
    case _ => throw AssertionError(s"Boolean expected, got $this")

  def asInt: Int = this match
    case VInt(value) => value
    case _ => throw AssertionError(s"Int expected, got $this")
  
  def asFloat: Double = this match
    case VFloat(value) => value
    case _ => throw AssertionError(s"Float expected, got $this")
  
  def asString: String = this match
    case VString(value) => value
    case _ => throw AssertionError(s"String expected, got $this")

  def convertToString: String = this match
    case VUnit => "()"
    case VBool(value) => value.toString
    case VInt(value) => value.toString
    case VFloat(value) => value.toString
    case VChar(value) => value.toString
    case VString(value) => value
    case UserDefinedFunction(position) => "<function>"
    case BuiltInFunction(f) => "<builtin function>"
  