/** A single call frame on the VM's call stack.
  *
  * Each function call (or class constructor invocation) pushes a new [[RuntimeFrame]] onto
  * the frame stack inside [[RuntimeContext]].  The frame holds everything that is private
  * to one activation of a function: the instruction pointer, the operand stack, and the
  * chain of nested variable scopes.
  */
package org.algorab.runtime

import kyo.Chunk
import org.algorab.ast.Identifier
import org.algorab.compiler.InstrPosition
import org.algorab.compiler.Value

/** An immutable call-frame snapshot.
  *
  * @param nextInstruction the instruction index to execute next
  * @param stack           the operand stack for this frame, stored innermost-first
  * @param scopes          the scope chain for this frame, innermost scope first;
  *                        the bottom-most scope always contains the closure captures
  *                        (or, for the root frame, the built-in bindings)
  */
case class RuntimeFrame(
    nextInstruction: InstrPosition,
    stack: Chunk[Value],
    scopes: Chunk[RuntimeScope]
):

  /** Returns a copy of this frame with the instruction pointer set to `nextInstruction`.
    *
    * @param nextInstruction the new instruction index
    */
  def jump(nextInstruction: InstrPosition): RuntimeFrame =
    this.copy(nextInstruction = nextInstruction)

  /** Returns a copy of this frame with `value` pushed onto the operand stack.
    *
    * @param value the value to push
    */
  def push(value: Value): RuntimeFrame =
    this.copy(stack = value +: stack)

  /** Returns the top-of-stack value and a copy of this frame with the stack popped.
    *
    * @return `(topValue, updatedFrame)`
    */
  def pop: (Value, RuntimeFrame) =
    (stack.head, this.copy(stack = stack.tail))

  /** Returns a copy of this frame with an empty scope pushed onto the scope chain. */
  def pushScope: RuntimeFrame =
    this.copy(scopes = RuntimeScope.empty +: scopes)

  /** Returns a copy of this frame with the innermost scope removed from the scope chain. */
  def popScope: RuntimeFrame =
    this.copy(scopes = scopes.tail)

  /** Looks up the value of `name` by scanning the scope chain from innermost to outermost.
    *
    * @param name the variable name
    * @return `Some(value)` if found in any scope, `None` otherwise
    */
  def getVariable(name: Identifier): Option[Value] =
    scopes.collectFirst(((scope: RuntimeScope) => scope.getVariable(name)).unlift)

  /** Returns a copy of this frame with `name` declared (initialised to `null`) in the
    * innermost scope.  If no scopes exist, a new one is created.
    *
    * @param name the variable name to declare
    */
  def declareVariable(name: Identifier): RuntimeFrame =
    this.copy(scopes = scopes.headOption match
      case Some(head) => RuntimeScope(head.variables + (name -> null)) +: scopes.tail
      case None       => RuntimeScope(Map(name -> null)) +: scopes
    )

  /** Returns a copy of this frame with `name` declared as a [[Value.VBox]] in the
    * innermost scope.  Used for mutable variables captured by closures.
    *
    * @param name the variable name to declare as a box
    */
  def declareBox(name: Identifier): RuntimeFrame =
    this.copy(scopes = scopes.headOption match
      case Some(head) => RuntimeScope(head.variables + (name -> Value.VBox(null))) +: scopes.tail
      case None       => RuntimeScope(Map(name -> Value.VBox(null))) +: scopes
    )

  /** Returns a copy of this frame with the first occurrence of `name` in the scope chain
    * updated to `value`.
    *
    * Scans from the innermost scope outward; throws [[AssertionError]] if the variable
    * is not found in any scope.
    *
    * @param name  the variable name to update
    * @param value the new value
    * @throws AssertionError if `name` is not declared in any scope
    */
  def assignVariable(name: Identifier, value: Value): RuntimeFrame =
    def rec(scopes: Chunk[RuntimeScope]): Chunk[RuntimeScope] = scopes match
      case head +: tail =>
        if head.variables.contains(name) then
          head.copy(variables = head.variables.updated(name, value)) +: tail
        else
          head +: rec(tail)

      case _ => throw AssertionError(s"Unknown variable: $name")

    this.copy(scopes = rec(scopes))

/** Factory for the root call frame. */
object RuntimeFrame:

  /** The initial call frame used when program execution starts.
    *
    * The instruction pointer starts at position 0 and the scope chain contains only
    * [[RuntimeScope.builtins]].
    */
  val root: RuntimeFrame = RuntimeFrame(
    nextInstruction = InstrPosition(0),
    stack = Chunk.empty,
    scopes = Chunk(RuntimeScope.builtins)
  )
