package org.algorab.parsing

import io.github.iltotore.pureparser.Span
import org.algorab.ast.Identifier

enum Token derives CanEqual:
  case LBool(value: Boolean, span: Span)
  case LInt(value: Int, span: Span)
  case LFloat(value: Double, span: Span)
  case LChar(value: Char, span: Span)
  case LString(value: String, span: Span)
  case Ident(identifier: Identifier, span: Span)
  case Indent(span: Span)
  case DeIndent(span: Span)
  case Newline(span: Span)

  // Symbols

  case ParenOpen(span: Span)
  case ParenClosed(span: Span)
  case Comma(span: Span)
  case Colon(span: Span)
  case Plus(span: Span)
  case Minus(span: Span)
  case Mul(span: Span)
  case Div(span: Span)
  case IntDiv(span: Span)
  case Percent(span: Span)
  case Equal(span: Span)
  case EqualEqual(span: Span)
  case NotEqual(span: Span)
  case Less(span: Span)
  case LessEqual(span: Span)
  case Greater(span: Span)
  case GreaterEqual(span: Span)

  // Keywords

  case And(span: Span)
  case Or(span: Span)
  case Not(span: Span)
  case If(span: Span)
  case Then(span: Span)
  case Else(span: Span)
  case For(span: Span)
  case While(span: Span)
  case Do(span: Span)
  case In(span: Span)
  case Def(span: Span)
  case Val(span: Span)
  case Mut(span: Span)

  def span: Span
