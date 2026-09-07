package org.algorab.ast

import io.github.iltotore.pureparser.Span

enum Instruction:
  case Push(value: Value, span: Span)
  
  case Not(span: Span)
  case Equal(span: Span)
  case NotEqual(span: Span)
  case Less(span: Span)
  case LessEqual(span: Span)
  case Greater(span: Span)
  case GreaterEqual(span: Span)
  case Minus(span: Span)
  case Add(span: Span)
  case Sub(span: Span)
  case Mul(span: Span)
  case Div(span: Span)
  case IntDiv(span: Span)
  case Mod(span: Span)
  case And(span: Span)
  case Or(span: Span)

  case Store(symbol: SymbolId, span: Span)
  case Load(symbol: SymbolId, span: Span)
  
  case Apply(paramCount: ParamCount, span: Span)
  case Jump(to: InstructionPosition, span: Span)
  case JumpIfFalse(to: InstructionPosition, span: Span)
  case Return(span: Span)

  def span: Span