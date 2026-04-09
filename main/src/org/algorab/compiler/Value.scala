/** Runtime value representations used by the compiler and virtual machine.
  *
  * A [[Value]] is either a primitive Algorab value (bool, int, float, char, string, array),
  * a heap-allocated object ([[VInstance]]), a mutable reference cell ([[VBox]]), a
  * class descriptor ([[VClass]]), or a callable function ([[UserDefinedFunction]] or
  * [[BuiltInFunction]]).
  *
  * `VBox` is used to implement mutable variables that are captured by closures: instead of
  * capturing the value directly, the closure captures the box, so both the outer scope and the
  * closure observe mutations made through the same box.
  */
package org.algorab.compiler

import java.io.IOException
import kyo.*
import org.algorab.ast.Identifier
import scala.collection.mutable

/** A value that can be held on the VM operand stack, stored in a variable, or passed as an argument.
  *
  * Unsafe cast methods (`asBool`, `asInt`, etc.) throw [[AssertionError]] when the value is
  * not of the expected variant; they are intended for use inside the VM where the type-checker
  * guarantees correctness.
  */
enum Value derives CanEqual:
  /** The singleton value of type `Unit`. */
  case VUnit

  /** A boolean value.
    *
    * @param value the underlying `Boolean`
    */
  case VBool(value: Boolean)

  /** A 32-bit signed integer value.
    *
    * @param value the underlying `Int`
    */
  case VInt(value: Int)

  /** A 64-bit floating-point value.
    *
    * @param value the underlying `Double`
    */
  case VFloat(value: Double)

  /** A single Unicode character value.
    *
    * @param value the underlying `Char`
    */
  case VChar(value: Char)

  /** An immutable string value.
    *
    * @param value the underlying `String`
    */
  case VString(value: String)

  /** A fixed-size array of [[Value]]s.
    *
    * @param values the underlying mutable JVM array
    */
  case VArray(values: Array[Value])

  /** A class descriptor (the class itself, not an instance).
    *
    * Calling a `VClass` value allocates a new `VInstance` and runs the constructor.
    *
    * @param name      the class's internal name
    * @param initStart the [[InstrPosition]] of the first constructor instruction
    */
  case VClass(name: Identifier, initStart: InstrPosition)

  /** A heap-allocated object instance.
    *
    * Fields are stored in a mutable map so that field assignments mutate the instance
    * in place, observable by all references that point to the same instance.
    *
    * @param className the class's internal name
    * @param fields    the instance's mutable field map
    */
  case VInstance(className: Identifier, fields: mutable.Map[Identifier, Value])

  /** A mutable reference cell wrapping another [[Value]].
    *
    * Used to implement captured mutable variables in closures.  The `var` field
    * allows in-place mutation via [[setBox]].
    *
    * @param boxxed the current wrapped value (initialised to `null` on declaration)
    */
  case VBox(var boxxed: Value)

  /** A user-defined function value, created when a [[org.algorab.compiler.Instruction.LoadFunction]]
    * instruction is executed.
    *
    * @param position     the [[InstrPosition]] of the first instruction in the function body
    * @param captured     a snapshot of the closed-over variable values at creation time
    */
  case UserDefinedFunction(position: InstrPosition, captured: Map[Identifier, Value])

  /** A built-in (JVM-implemented) function value.
    *
    * @param f a function from a `Chunk` of argument values to a `Value`, which may
    *          perform console I/O (`Sync`) or fail with [[java.io.IOException]]
    */
  case BuiltInFunction(f: Chunk[Value] => Value < (Sync & Abort[IOException]))

  // ── Unsafe accessors ──────────────────────────────────────────────────────

  /** Extracts the underlying `Boolean` from a [[VBool]].
    *
    * @return the boolean value
    * @throws AssertionError if this value is not a [[VBool]]
    */
  def asBool: Boolean = this match
    case VBool(value) => value
    case _            => throw AssertionError(s"Boolean expected, got $this")

  /** Extracts the underlying `Int` from a [[VInt]].
    *
    * @return the integer value
    * @throws AssertionError if this value is not a [[VInt]]
    */
  def asInt: Int = this match
    case VInt(value) => value
    case _           => throw AssertionError(s"Int expected, got $this")

  /** Extracts the underlying `Double` from a [[VFloat]].
    *
    * @return the double value
    * @throws AssertionError if this value is not a [[VFloat]]
    */
  def asFloat: Double = this match
    case VFloat(value) => value
    case _             => throw AssertionError(s"Float expected, got $this")

  /** Extracts the underlying `String` from a [[VString]].
    *
    * @return the string value
    * @throws AssertionError if this value is not a [[VString]]
    */
  def asString: String = this match
    case VString(value) => value
    case _              => throw AssertionError(s"String expected, got $this")

  /** Extracts the underlying `Array[Value]` from a [[VArray]].
    *
    * @return the array
    * @throws AssertionError if this value is not a [[VArray]]
    */
  def asArray: Array[Value] = this match
    case VArray(values) => values
    case _              => throw AssertionError(s"Array expected, got $this")

  // ── Field access ──────────────────────────────────────────────────────────

  /** Returns the value of field `name` on this instance.
    *
    * @param name the field name
    * @return the field value
    * @throws AssertionError if this value is not a [[VInstance]]
    */
  def getField(name: Identifier): Value = this match
    case VInstance(_, fields) => fields(name)
    case _                    => throw AssertionError(s"Object expected, got $this")

  /** Sets field `name` to `value` on this instance (mutates in place).
    *
    * @param name  the field name
    * @param value the new field value
    * @throws AssertionError if this value is not a [[VInstance]]
    */
  def putField(name: Identifier, value: Value): Unit = this match
    case VInstance(_, fields) => fields(name) = value
    case _                    => throw AssertionError(s"Object expected, got $this")

  // ── Box access ────────────────────────────────────────────────────────────

  /** Extracts the wrapped value from a [[VBox]].
    *
    * @return the inner value
    * @throws AssertionError if this value is not a [[VBox]]
    */
  def unbox: Value = this match
    case VBox(boxxed) => boxxed
    case _            => throw AssertionError(s"Box expected, got $this")

  /** Mutates the [[VBox]] in place, replacing the wrapped value.
    *
    * @param value the new inner value
    * @throws AssertionError if this value is not a [[VBox]]
    */
  def setBox(value: Value): Unit = this match
    case vbox: VBox => vbox.boxxed = value
    case _          => throw AssertionError(s"Box expected, got $this")

  // ── String conversion ─────────────────────────────────────────────────────

  /** Converts this value to the string representation shown by the built-in `println`.
    *
    * Notable behaviours:
    *   - [[VBox]] delegates to the inner value's representation.
    *   - [[VInstance]] omits the synthetic `this` field from the output.
    *   - [[UserDefinedFunction]] and [[BuiltInFunction]] print non-inspectable placeholders.
    *
    * @return a human-readable string representation
    */
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
