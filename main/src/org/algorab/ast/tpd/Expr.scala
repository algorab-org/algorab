package org.algorab.ast.tpd

import kyo.Chunk
import org.algorab.ast.Identifier
import org.algorab.ast.tpd.Type
import org.algorab.typer.VariableId

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

  case Minus(expr: Expr, exprType: Type)
  case Add(left: Expr, right: Expr, exprType: Type)
  case Sub(left: Expr, right: Expr, exprType: Type)
  case Mul(left: Expr, right: Expr, exprType: Type)
  case Div(left: Expr, right: Expr, exprType: Type)
  case IntDiv(left: Expr, right: Expr, exprType: Type)
  case Mod(left: Expr, right: Expr, exprType: Type)
  case And(left: Expr, right: Expr, exprType: Type)
  case Or(left: Expr, right: Expr, exprType: Type)

  case VarCall(id: VariableId, name: Identifier, exprType: Type)
  case ValDef(id: VariableId, name: Identifier, tpe: Type, expr: Expr, exprType: Type)
  case Assign(id: VariableId, name: Identifier, expr: Expr, exprType: Type)
  case Apply(expr: Expr, args: Chunk[Expr], exprType: Type)
  case FunRef(internalName: Identifier, exprType: Type)
  case ClassRef(internalName: Identifier, exprType: Type)
  case Select(id: VariableId, expr: Expr, name: Identifier, exprType: Type)
  case Block(declarations: Chunk[(VariableId, Identifier)], expressions: Chunk[Expr], exprType: Type)
  case If(cond: Expr, ifTrue: Expr, ifFalse: Expr, exprType: Type)
  case While(cond: Expr, body: Expr, exprType: Type)

  def exprType: Type

  def withType(tpe: Type): Expr = this match
    case LBool(value, _)                     => LBool(value, tpe)
    case LInt(value, _)                      => LInt(value, tpe)
    case LFloat(value, _)                    => LFloat(value, tpe)
    case LChar(value, _)                     => LChar(value, tpe)
    case LString(value, _)                   => LString(value, tpe)
    case Not(expr, _)                        => Not(expr, tpe)
    case Equal(left, right, _)               => Equal(left, right, tpe)
    case NotEqual(left, right, _)            => NotEqual(left, right, tpe)
    case Less(left, right, _1)               => Less(left, right, tpe)
    case LessEqual(left, right, _)           => LessEqual(left, right, tpe)
    case Greater(left, right, _)             => Greater(left, right, tpe)
    case GreaterEqual(left, right, _)        => GreaterEqual(left, right, tpe)
    case Minus(expr, _)                      => Minus(expr, tpe)
    case Add(left, right, _)                 => Add(left, right, tpe)
    case Sub(left, right, _)                 => Sub(left, right, tpe)
    case Mul(left, right, _)                 => Mul(left, right, tpe)
    case Div(left, right, _)                 => Div(left, right, tpe)
    case IntDiv(left, right, _)              => IntDiv(left, right, tpe)
    case Mod(left, right, _)                 => Mod(left, right, tpe)
    case And(left, right, _)                 => And(left, right, tpe)
    case Or(left, right, _)                  => Or(left, right, tpe)
    case VarCall(id, name, _)                => VarCall(id, name, tpe)
    case ValDef(id, name, typ, expr, _)      => ValDef(id, name, typ, expr, tpe)
    case Assign(id, name, expr, _)           => Assign(id, name, expr, tpe)
    case Apply(expr, args, _)                => Apply(expr, args, tpe)
    case FunRef(internalName, _)             => FunRef(internalName, tpe)
    case ClassRef(internalName, _)           => ClassRef(internalName, tpe)
    case Select(id, expr, name, _)           => Select(id, expr, name, tpe)
    case Block(declarations, expressions, _) => Block(declarations, expressions, tpe)
    case If(cond, ifTrue, ifFalse, _)        => If(cond, ifTrue, ifFalse, tpe)
    case While(cond, body, _)                => While(cond, body, tpe)
