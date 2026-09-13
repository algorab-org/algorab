package org.algorab.ast.compiled

import io.github.iltotore.pureparser.Span
import org.algorab.ast.InstructionPosition
import org.algorab.ast.ParamCount
import org.algorab.ast.SymbolId
import org.algorab.ast.Value

enum Instruction:
  case Push(value: Value, span: Span)

  case Not(span: Span)
  case Equal(span: Span)
  case NotEqual(span: Span)
  case ToFloat(span: Span)
  case LessInt(span: Span)
  case LessEqualInt(span: Span)
  case GreaterInt(span: Span)
  case GreaterEqualInt(span: Span)
  case MinusInt(span: Span)
  case AddInt(span: Span)
  case SubInt(span: Span)
  case MulInt(span: Span)
  case DivInt(span: Span)
  case IntDivInt(span: Span)
  case ModInt(span: Span)
  case LessFloat(span: Span)
  case LessEqualFloat(span: Span)
  case GreaterFloat(span: Span)
  case GreaterEqualFloat(span: Span)
  case MinusFloat(span: Span)
  case AddFloat(span: Span)
  case SubFloat(span: Span)
  case MulFloat(span: Span)
  case DivFloat(span: Span)
  case IntDivFloat(span: Span)
  case ModFloat(span: Span)
  case Store(symbol: SymbolId, span: Span)
  case StoreGlobal(symbol: SymbolId, span: Span)
  case Load(symbol: SymbolId, span: Span)
  case LoadGlobal(symbol: SymbolId, span: Span)

  case Apply(paramCount: ParamCount, span: Span)
  case Jump(to: InstructionPosition, span: Span)
  case JumpIfFalse(to: InstructionPosition, span: Span)
  case Return(span: Span)

  def span: Span
