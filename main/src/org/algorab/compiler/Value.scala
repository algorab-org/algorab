package org.algorab.compiler

import java.io.IOException
import kyo.*
import org.algorab.ast.Identifier
import scala.collection.mutable

enum Value derives CanEqual:
  case VUnit
  case VBool(value: Boolean)
  case VInt(value: Int)
  case VFloat(value: Double)
  case VChar(value: Char)
  case VString(value: String)
  case VArray(values: Array[Value])
  case VClass(name: Identifier, initStart: InstrPosition)
  case VInstance(className: Identifier, fields: mutable.Map[Identifier, Value])
  case VBox(var boxxed: Value)
  case UserDefinedFunction(position: InstrPosition, captured: Map[Identifier, Value])
  case BuiltInFunction(f: Chunk[Value] => Value < (Sync & Abort[IOException]))

  def asBool: Boolean = this match
    case VBool(value) => value
    case _            => throw AssertionError(s"Boolean expected, got $this")

  def asInt: Int = this match
    case VInt(value) => value
    case _           => throw AssertionError(s"Int expected, got $this")

  def asFloat: Double = this match
    case VFloat(value) => value
    case _             => throw AssertionError(s"Float expected, got $this")

  def asString: String = this match
    case VString(value) => value
    case _              => throw AssertionError(s"String expected, got $this")

  def asArray: Array[Value] = this match
    case VArray(values) => values
    case _              => throw AssertionError(s"Array expected, got $this")

  def getField(name: Identifier): Value = this match
    case VInstance(_, fields) => fields(name)
    case _                    => throw AssertionError(s"Object expected, got $this")

  def putField(name: Identifier, value: Value): Unit = this match
    case VInstance(_, fields) => fields(name) = value
    case _                    => throw AssertionError(s"Object expected, got $this")

  def unbox: Value = this match
    case VBox(boxxed) => boxxed
    case _            => throw AssertionError(s"Box expected, got $this")

  def setBox(value: Value): Unit = this match
    case vbox: VBox => vbox.boxxed = value
    case _          => throw AssertionError(s"Box expected, got $this")

  def convertToString: String = this match
    case VUnit                => "()"
    case VBool(value)         => value.toString
    case VInt(value)          => value.toString
    case VFloat(value)        => value.toString
    case VChar(value)         => value.toString
    case VString(value)       => value
    case VArray(values)       => values.mkString("Array(", ", ", ")")
    case VBox(boxxed)         => boxxed.convertToString
    case VClass(className, _) => s"<class $className>"
    case VInstance(className, fields) => fields.filterNot(_._1 == Identifier("this")).map((id, value) =>
        if value == null then s"$id = null"
        else s"$id = ${value.convertToString}"
      ).mkString(s"$className(", ", ", ")")
    case UserDefinedFunction(_, _) => "<function>"
    case BuiltInFunction(_)        => "<builtin function>"
