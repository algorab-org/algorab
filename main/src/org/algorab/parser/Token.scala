package org.algorab.parser

import org.algorab.ast.Identifier

enum Token derives CanEqual:
  case LBool(value: Boolean)
  case LInt(value: Int)
  case LFloat(value: Double)
  case LChar(value: Char)
  case LString(value: String)
  case Ident(identifier: Identifier)

  case Indent
  case DeIndent
  case Newline

  // Symbols
  case ParenOpen // (
  case ParenClosed // )
  case SquareOpen // [
  case SquareClosed // ]
  case BraceOpen // {
  case BraceClosed // }
  case Dot // .
  case Comma
  case Colon
  case Plus
  case Minus
  case Mul
  case Div
  case IntDiv
  case Percent
  case DoubleArrow // =>
  case Equal
  case EqualEqual
  case NotEqual
  case Less
  case LessEqual
  case Greater
  case GreaterEqual

  // Keywords
  case And
  case Or
  case Not
  case If
  case Then
  case Else
  case For
  case While
  case Do
  case In
  case Def
  case Val
  case Mut
  case Class
