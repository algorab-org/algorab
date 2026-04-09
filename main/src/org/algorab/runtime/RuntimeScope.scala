/** A single variable scope within a [[RuntimeFrame]].
  *
  * Scopes are pushed and popped by [[org.algorab.compiler.Instruction.PushScope]] and
  * [[org.algorab.compiler.Instruction.PopScope]].  Each scope is a flat map from variable
  * name to [[org.algorab.compiler.Value]].
  *
  * The [[RuntimeScope.builtins]] scope is always present at the bottom of the root frame and
  * provides the standard library functions (`println`, `readInt`, `readFloat`, `toFloat`,
  * `length`, `get`, `Array`).
  */
package org.algorab.runtime

import kyo.Console
import org.algorab.ast.Identifier
import org.algorab.compiler.Value

/** An immutable snapshot of a variable scope.
  *
  * @param variables the variable bindings in this scope, keyed by [[Identifier]]
  */
case class RuntimeScope(variables: Map[Identifier, Value]):

  /** Returns the value bound to `name` in this scope, if present.
    *
    * @param name the variable name to look up
    * @return `Some(value)` if found, `None` otherwise
    */
  def getVariable(name: Identifier): Option[Value] = variables.get(name)

object RuntimeScope:

  /** An empty scope with no variable bindings. */
  val empty: RuntimeScope = RuntimeScope(Map.empty)

  /** The built-in function scope, pre-populated with Algorab's standard library.
    *
    * Bindings provided:
    *   - `Unit`      – the [[Value.VUnit]] singleton.
    *   - `println`   – prints a value followed by a newline.
    *   - `readInt`   – reads a line from stdin and parses it as an `Int`.
    *   - `readFloat` – reads a line from stdin and parses it as a `Double`.
    *   - `toFloat`   – converts an `Int` to a `Float`.
    *   - `length`    – returns the length of an array.
    *   - `get`       – returns the element at a given index of an array.
    *   - `Array`     – constructs an array from its arguments.
    *
    * @note `length`, `get`, and `Array` are temporary implementations.  Once OOP and
    *       multi-file support are implemented they will be replaced by method calls.
    */
  val builtins: RuntimeScope = RuntimeScope(Map(
    Identifier("Unit") -> Value.VUnit,
    Identifier("println") -> Value.BuiltInFunction(args =>
      Console.printLine(args.head.convertToString).andThen(Value.VUnit)
    ),
    Identifier("readInt") -> Value.BuiltInFunction(args =>
      Console.readLine.map(str => Value.VInt(str.toInt))
    ),
    Identifier("readFloat") -> Value.BuiltInFunction(args =>
      Console.readLine.map(str => Value.VFloat(str.toDouble))
    ),
    Identifier("toFloat") -> Value.BuiltInFunction(args =>
      Value.VFloat(args.head.asInt.toDouble)
    ),
    // TODO Change length and Array once OOP and multifile are implemented
    Identifier("length") -> Value.BuiltInFunction(args =>
      Value.VInt(args.head.asArray.length)
    ),
    Identifier("get") -> Value.BuiltInFunction(args =>
      args(0).asArray(args(1).asInt)
    ),
    Identifier("Array") -> Value.BuiltInFunction(args =>
      Value.VArray(args.toArray)
    )
  ))
