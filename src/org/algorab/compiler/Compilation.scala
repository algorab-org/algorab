package org.algorab.compiler

import kyo.*

type Compilation = Env[InstrPosition] & Var[Chunk[Instruction]]

object Compilation:

  def run[S](offset: InstrPosition)(body: Unit < (Compilation & S)): Chunk[Instruction] < S =
    body.handle(
      Env.run(offset),
      Var.runTuple(Chunk.empty)
    ).map(_._1)

  def emit(instruction: Instruction): Unit < Compilation = Var.updateDiscard(_ :+ instruction)

  def emitAll(instructions: Chunk[Instruction]): Unit < Compilation = Var.updateDiscard(_ ++ instructions)

  def nextPosition: InstrPosition < Compilation =
    Env.get[InstrPosition].map(offset => Var.use(instrs => offset + instrs.size))