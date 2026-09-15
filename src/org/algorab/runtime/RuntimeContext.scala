package org.algorab.runtime

import org.algorab.ast.Value
import org.algorab.ast.SymbolId
import org.algorab.ast.compiled.Function
import org.algorab.ast.compiled.Module
import purelogic.*
import org.algorab.ast.InstructionPosition
import org.algorab.compilation.CompilationContext.currentPosition
import org.algorab.ast.compiled.Instruction
import io.github.iltotore.iron.autoRefine

case class RuntimeContext(
  frames: List[RuntimeFrame],
  modules: Map[SymbolId, Module],
  functions: Map[SymbolId, Function],
  globals: Map[SymbolId, Value]
)

object RuntimeContext:

  def default(
    modules: Map[SymbolId, Module],
    functions: Map[SymbolId, Function]
  ): RuntimeContext = RuntimeContext(
    frames = List.empty,
    modules = modules,
    functions = functions,
    globals = Map(
      SymbolId.UnitTerm -> Value(()),
      SymbolId.ToFloatTerm -> Value.BuiltinFunction:
        case Seq(value: Int) => Value(value.toFloat),
      SymbolId.PrintLnTerm -> Value.BuiltinFunction:
        case Seq(value) => Value(println(value))
    )
  )

  def currentFrame: Runtime[RuntimeFrame] = get.frames.head

  def updateCurrentFrame(f: RuntimeFrame => RuntimeFrame): Runtime[Unit] = update(ctx =>
    ctx.copy(frames = f(ctx.frames.head) :: ctx.frames.tail)
  )

  def modifyCurrentFrame[A](f: (RuntimeContext, RuntimeFrame) => (A, RuntimeFrame)): Runtime[A] = modify(ctx =>
    val (result, frame) = f(ctx, ctx.frames.head)
    (result, ctx.copy(frames = frame :: ctx.frames.tail))
  )

  def nextInstruction: Runtime[Instruction] = modifyCurrentFrame((ctx, frame) =>
    val function = ctx.functions(frame.currentFunction)
    val instruction = function.body(frame.position.value)
    (instruction, frame.copy(position = frame.position + 1))
  )

  def push(value: Value): Runtime[Unit] = updateCurrentFrame(ctx => ctx.copy(
    stack = value :: ctx.stack
  ))

  def pop: Runtime[Value] = modifyCurrentFrame((_, frame) =>
    (frame.stack.head, frame.copy(stack = frame.stack.tail))
  )

  def popN(n: Int): Runtime[List[Value]] = modifyCurrentFrame((_, frame) =>
    val (popped, remaining) = frame.stack.splitAt(n)
    (popped, frame.copy(stack = remaining))
  )

  def storeGlobal(symbol: SymbolId, value: Value): Runtime[Unit] = update(ctx => ctx.copy(
    globals = ctx.globals.updated(symbol, value)
  ))

  def loadGlobal(symbol: SymbolId): Runtime[Value] = get.globals(symbol)

  def storeLocal(symbol: SymbolId, value: Value): Runtime[Unit] = updateCurrentFrame(frame => frame.copy(
    variables = frame.variables.updated(symbol, value)
  ))

  def loadLocal(symbol: SymbolId): Runtime[Value] = currentFrame.variables(symbol)

  def jump(to: InstructionPosition): Runtime[Unit] = updateCurrentFrame(_.copy(position = to))

  def pushNewFrame(function: SymbolId, stack: List[Value]): Runtime[Unit] =
    if get.frames.sizeCompare(32) > 0 then throw AssertionError("Max recursion")
    update(frame => frame.copy(
      frames = RuntimeFrame.default(function, stack) :: frame.frames
    ))

  def popFrame(): Runtime[Unit] = update(frame => frame.copy(
    frames = frame.frames.tail
  ))

  def getModule(id: SymbolId): Runtime[Module] = get.modules(id)

  def isRunning: Runtime[Boolean] =
    val ctx = get
    val currentFrame = ctx.frames.head
    ctx.functions(currentFrame.currentFunction).body.sizeCompare(currentFrame.position.value) > 0