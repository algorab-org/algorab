package org.algorab.ast.tpd

import kyo.Chunk
import org.algorab.ast.Identifier
import org.algorab.ast.Type

enum Expr:
  case LBool(value: Boolean, exprType: Type)
  case LInt(value: Int, exprType: Type)
  case LFloat(value: Double, exprType: Type)
  case LChar(value: Char, exprType: Type)
  case LString(value: String, exprType: Type)

  case Not(expr: Expr, exprType: Type)
  case Equal(left: Expr, right: Expr, exprType: Type)
  case NotEqual(left: Expr, right: Expr, exprType: Type)
  case Less(left: Expr, right: Expr, exprType: Type)
  case LessEqual(left: Expr, right: Expr, exprType: Type)
  case Greater(left: Expr, right: Expr, exprType: Type)
  case GreaterEqual(left: Expr, right: Expr, exprType: Type)

  case Plus(expr: Expr, exprType: Type)
  case Minus(expr: Expr, exprType: Type)
  case Add(left: Expr, right: Expr, exprType: Type)
  case Sub(left: Expr, right: Expr, exprType: Type)
  case Mul(left: Expr, right: Expr, exprType: Type)
  case Div(left: Expr, right: Expr, exprType: Type)
  case IntDiv(left: Expr, right: Expr, exprType: Type)
  case Mod(left: Expr, right: Expr, exprType: Type)
  case And(left: Expr, right: Expr, exprType: Type)
  case Or(left: Expr, right: Expr, exprType: Type)

  case VarCall(name: Identifier, exprType: Type)
  case ValDef(name: Identifier, tpe: Type, expr: Expr, mutable: Boolean, exprType: Type)
  case Assign(name: Identifier, expr: Expr, exprType: Type)
  case Apply(expr: Expr, args: Chunk[Expr], exprType: Type)
  case FunDef(name: Identifier, typeParams: Chunk[Identifier], params: Chunk[(Identifier, Type)], retType: Type, body: Expr, exprType: Type)

  case Block(expressions: Chunk[Expr], exprType: Type)
  case If(cond: Expr, ifTrue: Expr, ifFalse: Expr, exprType: Type)
  case While(cond: Expr, body: Expr, exprType: Type)
  case For(iterator: Identifier, iterable: Expr, body: Expr, exprType: Type)

  def exprType: Type