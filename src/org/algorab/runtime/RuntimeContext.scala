package org.algorab.runtime

import io.github.iltotore.iron.autoRefine
import io.github.iltotore.pureparser.Span
import org.algorab.ast.InstructionPosition
import org.algorab.ast.SymbolId
import org.algorab.ast.Value
import org.algorab.ast.compiled.Function
import org.algorab.ast.compiled.Instruction
import org.algorab.ast.compiled.Module
import org.algorab.compilation.CompilationContext.currentPosition
import org.algorab.util.Console
import purelogic.*

/**
 * The runtime state of the virtual machine.
 *
 * @param frames the current call stack
 * @param modules the loaded modules
 * @param functions the compiled functions
 * @param owners the owning module of each global variable
 * @param globals the global variables
 * @param initializedModules the set of already initialized modules to prevent double-initialization
 */
case class RuntimeContext(
    frames: List[RuntimeFrame],
    modules: Map[SymbolId, Module],
    functions: Map[SymbolId, Function],
    owners: Map[SymbolId, SymbolId],
    globals: Map[SymbolId, Value],
    initializedModules: Set[SymbolId]
)

object RuntimeContext:

  /**
   * Create a default runtime context.
   *
   * @param modules the modules to load
   * @param functions the compiled functions
   * @param owners the owning module of each global variable
   * @return the initialized runtime context
   */
  def default(
      modules: Map[SymbolId, Module],
      functions: Map[SymbolId, Function],
      owners: Map[SymbolId, SymbolId]
  ): RuntimeContext = RuntimeContext(
    frames = List.empty,
    modules = modules,
    functions = functions,
    owners = owners,
    globals = Map(
      SymbolId.UnitTerm -> Value(()),
      SymbolId.ToFloatTerm -> Value.BuiltinFunction:
        case Seq(value: Int) => Value(value.toFloat),
      SymbolId.PrintLnTerm -> Value.BuiltinFunction:
        case Seq(value) => Value(Console.println(value.toString)),
      SymbolId.ReadIntTerm -> Value.BuiltinFunction:
        case Seq() => Value(Runtime.convertConsoleError(Console.readInt())),
      SymbolId.ReadFloatTerm -> Value.BuiltinFunction:
        case Seq() => Value(Runtime.convertConsoleError(Console.readDouble()))
    ),
    initializedModules = Set(SymbolId.Invalid)
  )

  /**
   * Get the current call frame.
   *
   * @return the current frame
   */
  def currentFrame: Runtime[RuntimeFrame] = get.frames.head

  /**
   * Update the current call frame.
   *
   * @param f the function used to update the frame
   */
  def updateCurrentFrame(f: RuntimeFrame => RuntimeFrame): Runtime[Unit] = update(ctx =>
    ctx.copy(frames = f(ctx.frames.head) :: ctx.frames.tail)
  )

  /**
   * Modify the current call frame and return a result.
   *
   * @param f the function used to modify the frame
   * @return the resulting value
   */
  def modifyCurrentFrame[A](f: (RuntimeContext, RuntimeFrame) => (A, RuntimeFrame)): Runtime[A] = modify(ctx =>
    val (result, frame) = f(ctx, ctx.frames.head)
    (result, ctx.copy(frames = frame :: ctx.frames.tail))
  )

  /**
   * Get and advance to the next instruction.
   *
   * @return the next instruction
   */
  def nextInstruction: Runtime[Instruction] =
    val ctx = get
    val frame = currentFrame

    val function = ctx.functions(frame.currentFunction)
    if frame.position.value < function.body.length then
      updateCurrentFrame(frame => frame.copy(position = frame.position + 1))
      function.body(frame.position.value)
    else
      popFrame()
      val newFrame = currentFrame
      ctx.functions(newFrame.currentFunction).body(newFrame.position.value - 1)

  /**
   * Push a value onto the current frame's stack.
   *
   * @param value the value to push
   */
  def push(value: Value): Runtime[Unit] = updateCurrentFrame(ctx =>
    ctx.copy(
      stack = value :: ctx.stack
    )
  )

  /**
   * Pop a value from the current frame's stack.
   *
   * @return the popped value
   */
  def pop: Runtime[Value] = modifyCurrentFrame((_, frame) =>
    (frame.stack.head, frame.copy(stack = frame.stack.tail))
  )

  /**
   * Pop multiple values from the current frame's stack.
   *
   * @param n the number of values to pop
   * @return the popped values
   */
  def popN(n: Int): Runtime[List[Value]] = modifyCurrentFrame((_, frame) =>
    val (popped, remaining) = frame.stack.splitAt(n)
    (popped, frame.copy(stack = remaining))
  )

  /**
   * Store a value in the global environment.
   *
   * @param symbol the symbol identifying the global variable
   * @param value the value to store
   */
  def storeGlobal(symbol: SymbolId, value: Value): Runtime[Unit] = update(ctx =>
    ctx.copy(
      globals = ctx.globals.updated(symbol, value)
    )
  )

  /**
   * Load a value from the global environment.
   *
   * @param symbol the symbol identifying the global variable
   * @return the stored value
   */
  def loadGlobal(symbol: SymbolId): Runtime[Value] = get.globals(symbol)

  /**
   * Store a value in the current frame.
   *
   * @param symbol the symbol identifying the local variable
   * @param value the value to store
   */
  def storeLocal(symbol: SymbolId, value: Value): Runtime[Unit] = updateCurrentFrame(frame =>
    frame.copy(
      variables = frame.variables.updated(symbol, value)
    )
  )

  /**
   * Load a value from the current frame.
   *
   * @param symbol the symbol identifying the local variable
   * @return the stored value
   */
  def loadLocal(symbol: SymbolId): Runtime[Value] = currentFrame.variables(symbol)

  /**
   * Jump to an instruction position in the current frame.
   *
   * @param to the instruction position to jump to
   */
  def jump(to: InstructionPosition): Runtime[Unit] = updateCurrentFrame(_.copy(position = to))

  /**
   * Push a new call frame.
   *
   * @param function the function to execute
   * @param stack the initial stack of the new frame
   */
  def pushNewFrame(function: SymbolId, stack: List[Value]): Runtime[Unit] =
    if get.frames.sizeCompare(32) > 0 then throw AssertionError("Max recursion")
    update(frame =>
      frame.copy(
        frames = RuntimeFrame.default(function, stack) :: frame.frames
      )
    )

  /**
   * Remove the current call frame.
   */
  def popFrame(): Runtime[Unit] = update(frame =>
    frame.copy(
      frames = frame.frames.tail
    )
  )

  /**
   * Get a compiled module.
   *
   * @param id the symbol identifying the module
   * @return the requested module
   */
  def getModule(id: SymbolId): Runtime[Module] = get.modules(id)

  /**
   * Whether the current function has more instructions to execute.
   */
  def isRunning: Runtime[Boolean] =
    val ctx = get
    val currentFrame = ctx.frames.head
    currentFrame.currentFunction != SymbolId.Root
    || ctx.functions(currentFrame.currentFunction).body.sizeCompare(currentFrame.position.value) > 0

  /**
   * The source span of the current instruction.
   */
  def currentSpan: Runtime[Span] =
    val ctx = get
    val frame = ctx.frames.head
    val function = ctx.functions(frame.currentFunction)
    val instruction = function.body(frame.position.value)
    instruction.span
