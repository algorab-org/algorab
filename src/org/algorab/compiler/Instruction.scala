package org.algorab.compiler

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

object Instruction:
  def declare(name: Identifier, boxxed: Boolean): Instruction =
    if boxxed then Instruction.DeclareBox(name)
    else Instruction.Declare(name)

  def assign(name: Identifier, boxxed: Boolean): Instruction =
    if boxxed then Instruction.AssignBox(name)
    else Instruction.Assign(name)

  def load(name: Identifier, boxxed: Boolean): Instruction =
    if boxxed then Instruction.LoadBox(name)
    else Instruction.Load(name)