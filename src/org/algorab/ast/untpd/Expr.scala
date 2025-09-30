package org.algorab.ast.untpd

import kyo.Chunk
import org.algorab.ast.Identifier
import org.algorab.ast.Type

enum Expr:
  case LBool(value: Boolean)
  case LInt(value: Int)
  case LFloat(value: Double)
  case LChar(value: Char)
  case LString(value: String)

  case Not(expr: Expr)
  case Equal(left: Expr, right: Expr)
  case NotEqual(left: Expr, right: Expr)
  case Less(left: Expr, right: Expr)
  case LessEqual(left: Expr, right: Expr)
  case Greater(left: Expr, right: Expr)
  case GreaterEqual(left: Expr, right: Expr)

  case Plus(expr: Expr)
  case Minus(expr: Expr)
  case Add(left: Expr, right: Expr)
  case Sub(left: Expr, right: Expr)
  case Mul(left: Expr, right: Expr)
  case Div(left: Expr, right: Expr)
  case IntDiv(left: Expr, right: Expr)
  case Mod(left: Expr, right: Expr)
  case And(left: Expr, right: Expr)
  case Or(left: Expr, right: Expr)

  case VarCall(name: Identifier)
  case ValDef(name: Identifier, tpe: Type, expr: Expr)
  case Assign(name: Identifier, expr: Expr)
  case Apply(expr: Expr, args: Chunk[Expr])
  case FunDef(name: Identifier, typeParams: Chunk[Identifier], params: Chunk[(Identifier, Type)], retType: Type, body: Expr)

  case Block(expressions: Chunk[Expr])
  case If(cond: Expr, ifTrue: Expr, ifFalse: Expr)
  case While(cond: Expr, body: Expr)
  case For(iterator: Identifier, iterable: Expr, body: Expr)
