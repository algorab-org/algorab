package org.algorab.compiler

import kyo.*

type Compilation = Var[Chunk[Instruction]]

object Compilation:

  def run[S](body: Unit < (Compilation & S)): Chunk[Instruction] < S = Var.runTuple(Chunk.empty)(body).map(_._1)

  def emit(instruction: Instruction): Unit < Compilation = Var.updateDiscard(_ :+ instruction)

  def emitAll(instructions: Chunk[Instruction]): Unit < Compilation = Var.updateDiscard(_ ++ instructions)

  def nextPosition: InstrPosition < Compilation = Var.use(instrs => InstrPosition.assume(instrs.size))