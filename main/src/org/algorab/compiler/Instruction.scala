package org.algorab.compiler

import kyo.Chunk
import org.algorab.ast.Identifier

enum Instruction derives CanEqual:
  case Push(value: Value)
  case Declare(name: Identifier)
  case DeclareBox(name: Identifier)
  case Assign(name: Identifier)
  case AssignBox(name: Identifier)
  case Load(name: Identifier)
  case LoadBox(name: Identifier)
  case LoadFunction(name: Identifier)
  case LoadClass(name: Identifier)
  case DeclareField(name: Identifier)
  case AssignField(name: Identifier)
  case Select(name: Identifier)

  case Not
  case Equal
  case NotEqual
  case Less
  case LessEqual
  case Greater
  case GreaterEqual

  case Minus
  case Add
  case Sub
  case Mul
  case Div
  case IntDiv
  case Mod
  case And
  case Or

  case Apply(paramCount: ParamCount)
  case Jump(position: InstrPosition)
  case JumpIf(ifTrue: InstrPosition, ifFalse: InstrPosition)
  case Return
  case PushScope
  case PopScope
  case FunctionStart(internalName: Identifier, displayName: Identifier, captures: Set[Identifier], next: InstrPosition)
  case ClassStart(internalName: Identifier, displayName: Identifier, next: InstrPosition)

object Instruction:
  def loadThis: Instruction = Instruction.Load(Identifier("this"))

  def declare(name: Identifier, boxxed: Boolean, field: Boolean): Chunk[Instruction] =
    if field then Chunk(loadThis, Instruction.DeclareField(name))
    else if boxxed then Chunk(Instruction.DeclareBox(name))
    else Chunk(Instruction.Declare(name))

  def assign(name: Identifier, boxxed: Boolean, field: Boolean): Chunk[Instruction] =
    if field then Chunk(loadThis, Instruction.AssignField(name))
    else if boxxed then Chunk(Instruction.AssignBox(name))
    else Chunk(Instruction.Assign(name))

  def load(name: Identifier, boxxed: Boolean, field: Boolean): Chunk[Instruction] =
    if field then Chunk(loadThis, Instruction.Select(name))
    else if boxxed then Chunk(Instruction.LoadBox(name))
    else Chunk(Instruction.Load(name))
