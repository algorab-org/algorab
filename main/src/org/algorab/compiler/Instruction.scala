/** Bytecode instruction set for the Algorab virtual machine.
  *
  * Instructions are stored in a flat, indexed `Chunk[Instruction]` and executed by
  * [[org.algorab.runtime.VM]].  The design is that of a simple stack machine:
  * most instructions pop their operands from the operand stack and push their result.
  *
  * Jump instructions use absolute [[InstrPosition]] values (pre-computed by the
  * [[Compiler]]) rather than relative offsets, which makes the code straightforward
  * to generate but means that any re-ordering of the instruction stream would require
  * updating all jump targets.
  */
package org.algorab.compiler

import kyo.Chunk
import org.algorab.ast.Identifier

/** A single VM instruction.
  *
  * Instructions are grouped by function:
  *   - '''Stack manipulation''': [[Push]]
  *   - '''Variable management''': [[Declare]], [[DeclareBox]], [[Assign]], [[AssignBox]],
  *     [[Load]], [[LoadBox]]
  *   - '''Function / class management''': [[LoadFunction]], [[LoadClass]],
  *     [[FunctionStart]], [[ClassStart]]
  *   - '''Field access''': [[DeclareField]], [[AssignField]], [[Select]]
  *   - '''Arithmetic / logical operators''': [[Not]], [[Equal]], [[NotEqual]], [[Less]],
  *     [[LessEqual]], [[Greater]], [[GreaterEqual]], [[Minus]], [[Add]], [[Sub]], [[Mul]],
  *     [[Div]], [[IntDiv]], [[Mod]], [[And]], [[Or]]
  *   - '''Control flow''': [[Apply]], [[Jump]], [[JumpIf]], [[Return]]
  *   - '''Scope management''': [[PushScope]], [[PopScope]]
  */
enum Instruction derives CanEqual:
  // ── Stack manipulation ─────────────────────────────────────────────────────

  /** Pushes a constant [[Value]] onto the operand stack.
    *
    * @param value the constant to push
    */
  case Push(value: Value)

  // ── Variable management ────────────────────────────────────────────────────

  /** Declares a new unboxed (direct-value) variable in the innermost scope, initialised to `null`.
    *
    * @param name the variable name
    */
  case Declare(name: Identifier)

  /** Declares a new boxed (mutable reference) variable in the innermost scope.
    *
    * The variable is stored as a `VBox(null)` wrapper.  Subsequent loads go through
    * [[LoadBox]] and writes go through [[AssignBox]].
    *
    * @param name the variable name
    */
  case DeclareBox(name: Identifier)

  /** Pops the top of the stack and assigns it to an existing unboxed variable.
    *
    * @param name the target variable name
    */
  case Assign(name: Identifier)

  /** Pops the top of the stack and stores it inside the `VBox` of an existing boxed variable.
    *
    * @param name the target (boxed) variable name
    */
  case AssignBox(name: Identifier)

  /** Pushes the value of an unboxed variable onto the stack.
    *
    * @param name the source variable name
    */
  case Load(name: Identifier)

  /** Pushes the unboxed inner value of a boxed variable onto the stack.
    *
    * @param name the source (boxed) variable name
    */
  case LoadBox(name: Identifier)

  // ── Function / class references ────────────────────────────────────────────

  /** Pushes a [[Value.UserDefinedFunction]] for the named function onto the stack.
    *
    * The function's captured variables are snapshot from the current runtime context.
    *
    * @param name the internal function name (as assigned by the typer)
    */
  case LoadFunction(name: Identifier)

  /** Pushes a [[Value.VClass]] value for the named class onto the stack.
    *
    * @param name the internal class name (as assigned by the typer)
    */
  case LoadClass(name: Identifier)

  // ── Field access ───────────────────────────────────────────────────────────

  /** Pops the top-of-stack instance and declares a new field `name` on it, set to `null`.
    *
    * @param name the field name
    */
  case DeclareField(name: Identifier)

  /** Pops the top-of-stack value and the instance beneath it, and stores the value as field `name`.
    *
    * @param name the field name
    */
  case AssignField(name: Identifier)

  /** Pops the top-of-stack instance and pushes the value of its field `name`.
    *
    * @param name the field name to select
    */
  case Select(name: Identifier)

  // ── Unary operators ────────────────────────────────────────────────────────

  /** Pops a boolean and pushes its logical negation. */
  case Not

  // ── Binary comparison operators ────────────────────────────────────────────

  /** Pops two values and pushes `true` iff they are structurally equal. */
  case Equal

  /** Pops two values and pushes `true` iff they are not structurally equal. */
  case NotEqual

  /** Pops two numeric values and pushes `true` iff the first-pushed is less than the second. */
  case Less

  /** Pops two numeric values and pushes `true` iff the first-pushed is ≤ the second. */
  case LessEqual

  /** Pops two numeric values and pushes `true` iff the first-pushed is greater than the second. */
  case Greater

  /** Pops two numeric values and pushes `true` iff the first-pushed is ≥ the second. */
  case GreaterEqual

  // ── Arithmetic operators ───────────────────────────────────────────────────

  /** Pops a numeric value and pushes its arithmetic negation. */
  case Minus

  /** Pops two numeric (or string) values and pushes their sum (or concatenation). */
  case Add

  /** Pops two numeric values and pushes their difference. */
  case Sub

  /** Pops two numeric values and pushes their product. */
  case Mul

  /** Pops two numeric values and pushes their floating-point quotient. */
  case Div

  /** Pops two numeric values and pushes their truncating-integer quotient. */
  case IntDiv

  /** Pops two numeric values and pushes the remainder. */
  case Mod

  /** Pops two booleans and pushes their logical AND. */
  case And

  /** Pops two booleans and pushes their logical OR. */
  case Or

  // ── Control flow ───────────────────────────────────────────────────────────

  /** Pops a callee and `paramCount` arguments, then invokes the callee.
    *
    * - For a [[Value.UserDefinedFunction]] or [[Value.VClass]], pushes a new [[org.algorab.runtime.RuntimeFrame]].
    * - For a [[Value.BuiltInFunction]], executes the built-in immediately and pushes the result.
    *
    * Arguments are popped in reverse order (rightmost first) and passed in order to the callee.
    *
    * @param paramCount the number of arguments to pop from the stack
    */
  case Apply(paramCount: ParamCount)

  /** Unconditionally sets the instruction pointer to `position`.
    *
    * @param position the target instruction index
    */
  case Jump(position: InstrPosition)

  /** Pops a boolean and jumps to `ifTrue` if it is `true`, otherwise jumps to `ifFalse`.
    *
    * @param ifTrue  the instruction index to jump to when the condition is `true`
    * @param ifFalse the instruction index to jump to when the condition is `false`
    */
  case JumpIf(ifTrue: InstrPosition, ifFalse: InstrPosition)

  /** Pops the return value, pops the current [[org.algorab.runtime.RuntimeFrame]], and pushes the
    * return value onto the caller's stack. */
  case Return

  // ── Scope management ──────────────────────────────────────────────────────

  /** Pushes a new empty variable scope onto the current frame's scope stack. */
  case PushScope

  /** Pops the innermost variable scope from the current frame's scope stack. */
  case PopScope

  // ── Definition markers ─────────────────────────────────────────────────────

  /** Registers a function definition in the runtime and jumps past its body.
    *
    * When executed, this instruction:
    *   1. Records the function (with its display name, capture list, and entry point)
    *      via `RuntimeContext.declareFunction`.
    *   1. Jumps to `next`, skipping the function body so it is not executed inline.
    *
    * @param internalName the unique internal name used as a key in the function table
    * @param displayName  the source-level name shown in diagnostics
    * @param captures     the set of local variable names that this function closes over
    * @param next         the instruction index immediately after the function body
    */
  case FunctionStart(
      internalName: Identifier,
      displayName: Identifier,
      captures: Set[Identifier],
      next: InstrPosition
  )

  /** Registers a class definition in the runtime and jumps past its constructor body.
    *
    * When executed, this instruction:
    *   1. Records the class (with its display name and constructor entry point)
    *      via `RuntimeContext.declareClass`.
    *   1. Jumps to `next`, skipping the constructor body so it is not executed inline.
    *
    * @param internalName the unique internal name used as a key in the class table
    * @param displayName  the source-level name shown in diagnostics
    * @param next         the instruction index immediately after the constructor body
    */
  case ClassStart(internalName: Identifier, displayName: Identifier, next: InstrPosition)

/** Instruction factory helpers for common multi-instruction sequences. */
object Instruction:
  /** Constructs a [[Load]] instruction for the synthetic `this` variable. */
  def loadThis: Instruction = Instruction.Load(Identifier("this"))

  /** Returns the instruction sequence for declaring a variable, considering boxing and field status.
    *
    * @param name   the variable name
    * @param boxxed `true` if the variable must be stored as a mutable box (captured mutable var)
    * @param field  `true` if the variable is a class field (uses [[DeclareField]] on `this`)
    * @return one or two instructions that together perform the declaration
    */
  def declare(name: Identifier, boxxed: Boolean, field: Boolean): Chunk[Instruction] =
    if field then Chunk(loadThis, Instruction.DeclareField(name))
    else if boxxed then Chunk(Instruction.DeclareBox(name))
    else Chunk(Instruction.Declare(name))

  /** Returns the instruction sequence for assigning a value to a variable, considering boxing and field status.
    *
    * Assumes the new value is already on top of the stack.
    *
    * @param name   the variable name
    * @param boxxed `true` if the variable is a mutable box
    * @param field  `true` if the variable is a class field
    * @return one or two instructions that perform the assignment
    */
  def assign(name: Identifier, boxxed: Boolean, field: Boolean): Chunk[Instruction] =
    if field then Chunk(loadThis, Instruction.AssignField(name))
    else if boxxed then Chunk(Instruction.AssignBox(name))
    else Chunk(Instruction.Assign(name))

  /** Returns the instruction sequence for loading a variable's value onto the stack,
    * considering boxing and field status.
    *
    * @param name   the variable name
    * @param boxxed `true` if the variable is a mutable box (inner value is extracted)
    * @param field  `true` if the variable is a class field
    * @return one or two instructions that push the variable's value
    */
  def load(name: Identifier, boxxed: Boolean, field: Boolean): Chunk[Instruction] =
    if field then Chunk(loadThis, Instruction.Select(name))
    else if boxxed then Chunk(Instruction.LoadBox(name))
    else Chunk(Instruction.Load(name))
