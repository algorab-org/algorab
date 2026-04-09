/** The full interpreter state for an Algorab program execution.
  *
  * [[RuntimeContext]] is a pure, immutable value that holds the entire state of the virtual
  * machine at any point during execution.  The Kyo `Var[RuntimeContext]` effect is used to
  * thread it through the interpreter; all modifications return a new context value.
  *
  * The companion object [[RuntimeContext$]] provides static combinators that operate through
  * the `Var[RuntimeContext]` effect without callers needing to read-modify-write the state
  * by hand.
  */
package org.algorab.runtime

import kyo.*
import org.algorab.ast.Identifier
import org.algorab.compiler.InstrPosition
import org.algorab.compiler.Value

/** Immutable snapshot of the complete VM state.
  *
  * @param frames    the call-frame stack; the head is the currently executing frame.
  *                  The stack is non-empty for as long as the program is running.
  * @param functions the global function table, populated by
  *                  [[org.algorab.compiler.Instruction.FunctionStart]] instructions.
  * @param classes   the global class table, populated by
  *                  [[org.algorab.compiler.Instruction.ClassStart]] instructions.
  */
case class RuntimeContext(
    frames: Chunk[RuntimeFrame],
    functions: Map[Identifier, FunctionDef],
    classes: Map[Identifier, ClassDef]
):

  /** Returns a copy with the head frame replaced by `f(head)`. */
  def modifyHeadFrame(f: RuntimeFrame => RuntimeFrame): RuntimeContext =
    this.copy(frames = f(frames.head) +: frames.tail)

  /** Applies `f` to the head frame, returning both the result value and the updated context. */
  def modifyHeadFrameReturn[A](f: RuntimeFrame => (A, RuntimeFrame)): (A, RuntimeContext) =
    val (result, updated) = f(frames.head)
    (result, this.copy(frames = updated +: frames.tail))

  /** Returns the instruction index that will be executed next (from the head frame). */
  def nextInstruction: InstrPosition = frames.head.nextInstruction

  /** Returns a copy with the head frame's instruction pointer set to `nextInstruction`. */
  def jump(nextInstruction: InstrPosition): RuntimeContext =
    modifyHeadFrame(_.jump(nextInstruction))

  /** Returns a copy with `value` pushed onto the head frame's operand stack. */
  def push(value: Value): RuntimeContext =
    modifyHeadFrame(_.push(value))

  /** Returns the top-of-stack value and a copy with the head frame's stack popped. */
  def pop: (Value, RuntimeContext) =
    modifyHeadFrameReturn(_.pop)

  /** Returns a copy with an empty scope pushed onto the head frame's scope chain. */
  def pushScope: RuntimeContext =
    modifyHeadFrame(_.pushScope)

  /** Returns a copy with the innermost scope of the head frame removed. */
  def popScope: RuntimeContext =
    modifyHeadFrame(_.popScope)

  /** Returns a copy with a new [[RuntimeFrame]] pushed on top of the call stack.
    *
    * Used when invoking a function or constructor.
    *
    * @param frame the new call frame to push
    */
  def pushFrame(frame: RuntimeFrame): RuntimeContext =
    this.copy(frames = frame +: frames)

  /** Returns a copy with the head call frame removed (function return). */
  def popFrame: RuntimeContext =
    this.copy(frames = frames.tail)

  /** Searches all frames' scope chains for a variable with `name`.
    *
    * @param name the variable to look up
    * @return `Some(value)` if found in any frame, `None` otherwise
    */
  def getVariable(name: Identifier): Option[Value] =
    frames.collectFirst(((frame: RuntimeFrame) => frame.getVariable(name)).unlift)

  /** Returns a copy with `name` declared in the head frame's innermost scope. */
  def declareVariable(name: Identifier): RuntimeContext =
    modifyHeadFrame(_.declareVariable(name))

  /** Returns a copy with `name` declared as a boxed variable in the head frame's innermost scope. */
  def declareBox(name: Identifier): RuntimeContext =
    modifyHeadFrame(_.declareBox(name))

  /** Returns a copy with `name` updated to `value` in the head frame's scope chain. */
  def assignVariable(name: Identifier, value: Value): RuntimeContext =
    modifyHeadFrame(_.assignVariable(name, value))

  /** Looks up a function descriptor by its internal name.
    *
    * @param name the function's internal name
    * @return `Some(FunctionDef)` if found, `None` otherwise
    */
  def getFunction(name: Identifier): Option[FunctionDef] = functions.get(name)

  /** Returns a copy with a new function registered under `name`. */
  def declareFunction(name: Identifier, function: FunctionDef): RuntimeContext =
    this.copy(functions = functions.updated(name, function))

  /** Looks up a class descriptor by its internal name.
    *
    * @param name the class's internal name
    * @return `Some(ClassDef)` if found, `None` otherwise
    */
  def getClass(name: Identifier): Option[ClassDef] = classes.get(name)

  /** Returns a copy with a new class registered under `name`. */
  def declareClass(name: Identifier, classDef: ClassDef): RuntimeContext =
    this.copy(classes = classes.updated(name, classDef))

/** Companion containing the static effect-style API for [[RuntimeContext]].
  *
  * All methods here operate through `Var[RuntimeContext]` so callers do not need to
  * manually read, modify, and write the state.
  */
object RuntimeContext:

  /** The initial context: a single root frame, no functions, no classes. */
  val empty: RuntimeContext = RuntimeContext(
    frames = Chunk(RuntimeFrame.root),
    functions = Map.empty,
    classes = Map.empty
  )

  /** Applies `f` to the current context and replaces it with the returned context.
    *
    * @param f a function from the current context to a new context
    */
  def modify(f: RuntimeContext => RuntimeContext < Runtime): Unit < Runtime =
    Var.use[RuntimeContext](f)
      .map(Var.set)
      .unit

  /** Applies `f` to the current context, returning the first component and updating the context
    * with the second component.
    *
    * @param f a function returning `(result, newContext)`
    * @tparam A the type of the result value
    * @return the result value `A`
    */
  def modifyReturn[A](f: RuntimeContext => (A, RuntimeContext) < Runtime): A < Runtime =
    Var.use[RuntimeContext](f)
      .map((result, newState) => Var.set(newState).andThen(result))

  /** Returns the next instruction index from the current context. */
  def nextInstruction: InstrPosition < Runtime = Var.use[RuntimeContext](_.nextInstruction)

  /** Sets the instruction pointer to `nextInstruction` in the current context. */
  def jump(nextInstruction: InstrPosition): Unit < Runtime = modify(_.jump(nextInstruction))

  /** Pushes `value` onto the current frame's operand stack. */
  def push(value: Value): Unit < Runtime = modify(_.push(value))

  /** Pops and returns the top value from the current frame's operand stack. */
  def pop: Value < Runtime = modifyReturn(_.pop)

  /** Pushes an empty scope onto the current frame's scope chain. */
  def pushScope: Unit < Runtime = modify(_.pushScope)

  /** Pops the innermost scope from the current frame's scope chain. */
  def popScope: Unit < Runtime = modify(_.popScope)

  /** Pushes a new call frame onto the frame stack. */
  def pushFrame(frame: RuntimeFrame): Unit < Runtime =
    modify(_.pushFrame(frame))

  /** Pops the current call frame (function return). */
  def popFrame: Unit < Runtime = modify(_.popFrame)

  /** Returns the value of `name`, searching all frames; throws [[AssertionError]] if not found.
    *
    * @param name the variable to look up
    * @throws AssertionError if `name` is not found in any frame
    */
  def getVariable(name: Identifier): Value < Runtime =
    Var.use[RuntimeContext](
      _
        .getVariable(name)
        .getOrElse(throw AssertionError(s"Unknown variable: $name"))
    )

  /** Declares `name` in the current frame's innermost scope. */
  def declareVariable(name: Identifier): Unit < Runtime =
    modify(_.declareVariable(name))

  /** Declares `name` as a boxed variable in the current frame's innermost scope. */
  def declareBox(name: Identifier): Unit < Runtime =
    modify(_.declareBox(name))

  /** Updates the value of `name` in the current frame's scope chain. */
  def assignVariable(name: Identifier, value: Value): Unit < Runtime =
    modify(_.assignVariable(name, value))

  /** Returns the [[FunctionDef]] for `name`; throws [[AssertionError]] if not found.
    *
    * @throws AssertionError if `name` is not in the function table
    */
  def getFunction(name: Identifier): FunctionDef < Runtime =
    Var.use[RuntimeContext](
      _
        .getFunction(name)
        .getOrElse(throw AssertionError(s"Unknown function: $name"))
    )

  /** Registers a new function in the function table. */
  def declareFunction(name: Identifier, function: FunctionDef): Unit < Runtime =
    modify(_.declareFunction(name, function))

  /** Returns the [[ClassDef]] for `name`; throws [[AssertionError]] if not found.
    *
    * @throws AssertionError if `name` is not in the class table
    */
  def getClass(name: Identifier): ClassDef < Runtime =
    Var.use[RuntimeContext](
      _
        .getClass(name)
        .getOrElse(throw AssertionError(s"Unknown class: $name"))
    )

  /** Registers a new class in the class table. */
  def declareClass(name: Identifier, classDef: ClassDef): Unit < Runtime =
    modify(_.declareClass(name, classDef))
