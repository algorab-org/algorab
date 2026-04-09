/** The compilation effect and its combinators.
  *
  * [[Compilation]] is a Kyo effect alias that bundles all the capabilities needed during
  * code generation:
  *   - `Env[TypeContext]` – read-only access to the type-checker's output (functions, classes,
  *     variable metadata).
  *   - `Env[InstrPosition]` – the absolute instruction-stream offset at which the current
  *     compilation sub-task starts (used to compute absolute jump targets).
  *   - `Var[Chunk[Instruction]]` – the mutable accumulator of emitted instructions.
  *
  * All public methods in this object are thin wrappers that make the effect ergonomic to
  * use from [[Compiler]] without spelling out the full effect type every time.
  */
package org.algorab.compiler

import kyo.*
import org.algorab.ast.Identifier
import org.algorab.typer.ClassTypeDef
import org.algorab.typer.FunctionDef
import org.algorab.typer.TypeContext
import org.algorab.typer.Variable
import org.algorab.typer.VariableId

/** Effect alias for the code-generation phase.
  *
  * Combines the full type context with the "no-context" effects needed to manage the
  * instruction stream.
  */
type Compilation = Env[TypeContext] & Compilation.NoContext

object Compilation:

  /** The sub-set of compilation effects that do not require the [[TypeContext]].
    *
    * Used when compiling sub-expressions in isolation (e.g. to pre-compute jump targets)
    * via [[run]].
    */
  type NoContext = Env[InstrPosition] & Var[Chunk[Instruction]]

  /** Runs a compilation sub-task in isolation and returns the emitted instructions.
    *
    * The sub-task starts at absolute position `offset`; instructions are accumulated
    * in a fresh `Chunk` which is returned when the effect completes.  This is used to
    * pre-compile branches (if/else, while body, and/or short-circuit) so their sizes
    * are known before absolute jump addresses can be computed.
    *
    * @param offset the absolute instruction position at which the sub-task begins
    * @param body   the compilation actions to run
    * @tparam S     any additional effects in `body`
    * @return the instructions emitted by `body`
    */
  def run[S](offset: InstrPosition)(body: Unit < (Compilation.NoContext & S)): Chunk[Instruction] < S =
    body.handle(
      Env.run(offset),
      Var.runTuple(Chunk.empty)
    ).map(_._1)

  /** Appends a single instruction to the instruction accumulator.
    *
    * @param instruction the instruction to emit
    */
  def emit(instruction: Instruction): Unit < Compilation = Var.updateDiscard(_ :+ instruction)

  /** Appends a chunk of instructions to the instruction accumulator.
    *
    * @param instructions the instructions to emit
    */
  def emitAll(instructions: Chunk[Instruction]): Unit < Compilation = Var.updateDiscard(_ ++ instructions)

  /** Returns the absolute instruction position of the ''next'' instruction to be emitted.
    *
    * Computed as `offset + numberOfInstructionsEmittedSoFar`.
    */
  def nextPosition: InstrPosition < Compilation =
    Env.get[InstrPosition].map(offset => Var.use(instrs => offset + instrs.size))

  /** Returns the map of all compiled functions from the [[TypeContext]].
    *
    * @return `Map[Identifier, FunctionDef]`
    */
  def functions: Map[Identifier, FunctionDef] < Compilation =
    Env.use[TypeContext](_.functions)

  /** Returns the map of all compiled classes from the [[TypeContext]].
    *
    * @return `Map[Identifier, ClassTypeDef]`
    */
  def classes: Map[Identifier, ClassTypeDef] < Compilation =
    Env.use[TypeContext](_.classes)

  /** Looks up the [[Variable]] metadata for the variable with the given [[VariableId]].
    *
    * @param id the variable's stable identifier
    * @return the corresponding [[Variable]] record
    */
  def getVariable(id: VariableId): Variable < Compilation =
    Env.use[TypeContext](_.variables(id.value))
