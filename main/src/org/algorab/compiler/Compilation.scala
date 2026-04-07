package org.algorab.compiler

import kyo.*
import org.algorab.ast.Identifier
import org.algorab.typer.FunctionDef
import org.algorab.typer.TypeContext
import org.algorab.typer.Variable
import org.algorab.typer.VariableId
import org.algorab.typer.ClassTypeDef

type Compilation = Env[TypeContext] & Compilation.NoContext

object Compilation:

  type NoContext = Env[InstrPosition] & Var[Chunk[Instruction]]

  def run[S](offset: InstrPosition)(body: Unit < (Compilation.NoContext & S)): Chunk[Instruction] < S =
    body.handle(
      Env.run(offset),
      Var.runTuple(Chunk.empty)
    ).map(_._1)

  def emit(instruction: Instruction): Unit < Compilation = Var.updateDiscard(_ :+ instruction)

  def emitAll(instructions: Chunk[Instruction]): Unit < Compilation = Var.updateDiscard(_ ++ instructions)

  def nextPosition: InstrPosition < Compilation =
    Env.get[InstrPosition].map(offset => Var.use(instrs => offset + instrs.size))

  def functions: Map[Identifier, FunctionDef] < Compilation =
    Env.use[TypeContext](_.functions)

  def classes: Map[Identifier, ClassTypeDef] < Compilation =
    Env.use[TypeContext](_.classes)

  def getVariable(id: VariableId): Variable < Compilation =
    Env.use[TypeContext](_.variables(id.value))
