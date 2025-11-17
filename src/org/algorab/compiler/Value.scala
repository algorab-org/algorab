package org.algorab.compiler

enum Value:
  case VBool(value: Boolean)
  case VInt(value: Int)
  case VFloat(value: Double)
  case VChar(value: Char)
  case VString(value: String)
  case Function(position: InstrPosition)