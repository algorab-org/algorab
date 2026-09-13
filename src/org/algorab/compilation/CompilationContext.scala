package org.algorab.compilation

import org.algorab.ast.compiled.Instruction
import org.algorab.ast.compiled.Function
import purelogic.*
import org.algorab.ast.InstructionPosition
import io.github.iltotore.iron.autoRefine
import io.github.iltotore.iron.assume
import org.algorab.ast.SymbolId
import io.github.iltotore.pureparser.Span

case class CompilationContext(
  functions: Map[SymbolId, Function],
  globals: Set[SymbolId],
  instructions: Seq[Instruction],
  position: InstructionPosition
)

object CompilationContext:

  val default: CompilationContext = CompilationContext(
    functions = Map.empty,
    globals = Set.empty,
    instructions = Seq.empty,
    position = InstructionPosition(0)
  )

  def emit(instruction: Instruction): Compilation[Unit] = update(ctx => ctx.copy(
    instructions = ctx.instructions :+ instruction,
    position = ctx.position + 1
  ))

  def emitAll(instructions: Seq[Instruction]): Compilation[Unit] = update(ctx => ctx.copy(
    instructions = ctx.instructions ++ instructions,
    position = ctx.position + instructions.size.assume
  ))

  def currentPosition: Compilation[InstructionPosition] = get.position

  def addFunction(symbol: SymbolId, function: Function): Compilation[Unit] = update(ctx => ctx.copy(
    functions = ctx.functions.updated(symbol, function)
  ))

  def isGlobal(symbol: SymbolId): Compilation[Boolean] = get.globals.contains(symbol)

  def declareGlobal(symbol: SymbolId): Compilation[Unit] = update(ctx => ctx.copy(
    globals = ctx.globals + symbol
  ))

  def emitStore(symbol: SymbolId, span: Span): Compilation[Unit] =
    if isGlobal(symbol) then emit(Instruction.StoreGlobal(symbol, span))
    else emit(Instruction.Store(symbol, span))

  def emitLoad(symbol: SymbolId, span: Span): Compilation[Unit] =
    if isGlobal(symbol) then emit(Instruction.LoadGlobal(symbol, span))
    else emit(Instruction.Load(symbol, span))