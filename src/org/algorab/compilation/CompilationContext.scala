package org.algorab.compilation

import org.algorab.ast.compiled.Instruction
import org.algorab.ast.compiled.Function
import purelogic.*
import org.algorab.ast.InstructionPosition
import io.github.iltotore.iron.autoRefine
import io.github.iltotore.iron.assume
import org.algorab.ast.SymbolId
import io.github.iltotore.pureparser.Span

/**
  * The context used during the compilation phase.
  *
  * @param functions the compiled functions
  * @param globals the definitions marked as global, usually top-level ones
  * @param instructions the current instruction body being produced
  * @param position the position of the next instruction, can be different from `instructions`' size
  */
case class CompilationContext(
  functions: Map[SymbolId, Function],
  globals: Set[SymbolId],
  instructions: Seq[Instruction],
  position: InstructionPosition
)

object CompilationContext:

  /**
    * The default compilation context.
    * Contains standard symbols.
    */
  val default: CompilationContext = CompilationContext(
    functions = Map.empty,
    globals = Set(
      SymbolId.UnitTerm,
      SymbolId.ToFloatTerm,
      SymbolId.PrintLnTerm,
      SymbolId.ReadIntTerm,
      SymbolId.ReadFloatTerm
    ),
    instructions = Seq.empty,
    position = InstructionPosition(0)
  )

  /**
    * Emit an instruction.
    *
    * @param instruction the instruction to emit
    */
  def emit(instruction: Instruction): Compilation[Unit] = update(ctx => ctx.copy(
    instructions = ctx.instructions :+ instruction,
    position = ctx.position + 1
  ))

  /**
    * Emit several instructions.
    *
    * @param instructions the instructions to emit
    */
  def emitAll(instructions: Seq[Instruction]): Compilation[Unit] = update(ctx => ctx.copy(
    instructions = ctx.instructions ++ instructions,
    position = ctx.position + instructions.size.assume
  ))

  /**
    * The position of the next instruction to emit.
    */
  def currentPosition: Compilation[InstructionPosition] = get.position

  /**
    * Add a compiled function.
    *
    * @param symbol the function's symbol
    * @param function the compiled form of the function
    */
  def addFunction(symbol: SymbolId, function: Function): Compilation[Unit] = update(ctx => ctx.copy(
    functions = ctx.functions.updated(symbol, function)
  ))

  /**
    * Check if the given definition is global.
    *
    * @param symbol the symbol of the definition to check
    * @return `true` if the definition is marked as global
    */
  def isGlobal(symbol: SymbolId): Compilation[Boolean] = get.globals.contains(symbol)

  /**
    * Mark a definition as global.
    *
    * @param symbol the symbol of the definition to declare as global
    */
  def declareGlobal(symbol: SymbolId): Compilation[Unit] = update(ctx => ctx.copy(
    globals = ctx.globals + symbol
  ))

  /**
    * Emit an [[Instruction.Store]] or an [[Instruction.StoreGlobal]] depending on
    * whether the definition to store into is global or not.
    *
    * @param symbol the symbol of the definition to store into
    * @param span the source position of the instruction to emit
    */
  def emitStore(symbol: SymbolId, span: Span): Compilation[Unit] =
    if isGlobal(symbol) then emit(Instruction.StoreGlobal(symbol, span))
    else emit(Instruction.Store(symbol, span))

  /**
    * Emit an [[Instruction.Load]] or an [[Instruction.LoadGlobal]] depending on
    * whether the definition to load from is global or not.
    *
    * @param symbol the symbol of the definition to load from
    * @param span the source position of the instruction to emit
    */
  def emitLoad(symbol: SymbolId, span: Span): Compilation[Unit] =
    if isGlobal(symbol) then emit(Instruction.LoadGlobal(symbol, span))
    else emit(Instruction.Load(symbol, span))