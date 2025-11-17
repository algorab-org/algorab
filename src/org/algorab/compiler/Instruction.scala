package org.algorab.compiler

import org.algorab.ast.Identifier

/* 
while condition do
  println("hey")

AST:
  While(
    VarCall("condition"),
    Apply(
      VarCall("println"),
      LString("hey")
    )
  )

Instructions:
  Load("condition")
  JumpIf(2, 6)
  Load("println")
  Push("hey")
  Apply(1)
  Jump(0)
  ...
*/

enum Instruction:
  case Push(value: Value)
  case Store(name: Identifier)
  case Load(name: Identifier)

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
  case PushScope
  case PopScope