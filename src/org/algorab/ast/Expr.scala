package org.algorab.ast

import io.github.iltotore.pureparser.Span

enum Expr:
  case LBool(value: Boolean, span: Span)
  case LInt(value: Int, span: Span)
  case LFloat(value: Double, span: Span)
  case LChar(value: Char, span: Span)
  case LString(value: String, span: Span)
  case Not(expr: Expr, span: Span)
  case Equal(left: Expr, right: Expr, span: Span)
  case NotEqual(left: Expr, right: Expr, span: Span)
  case Less(left: Expr, right: Expr, span: Span)
  case LessEqual(left: Expr, right: Expr, span: Span)
  case Greater(left: Expr, right: Expr, span: Span)
  case GreaterEqual(left: Expr, right: Expr, span: Span)
  case Plus(expr: Expr, span: Span)
  case Minus(expr: Expr, span: Span)  
  case Add(left: Expr, right: Expr, span: Span)  
  case Sub(left: Expr, right: Expr, span: Span)  
  case Mul(left: Expr, right: Expr, span: Span)  
  case Div(left: Expr, right: Expr, span: Span)
  case IntDiv(left: Expr, right: Expr, span: Span)  
  case Mod(left: Expr, right: Expr, span: Span)  
  case And(left: Expr, right: Expr, span: Span)  
  case Or(left: Expr, right: Expr, span: Span)    
  case VarCall(name: Identifier, span: Span)
  case ValDef(name: Identifier, tpe: Type, expr: Expr, mutable: Boolean, span: Span) 
  case Assign(name: Identifier, expr: Expr, span: Span)
  case Apply(expr: Expr, args: List[Expr], span: Span)
  case FunDef(name: Identifier, params: List[(Identifier, Type)], retType: Type, body: Expr, span: Span)
  case Block(expressions: List[Expr], span: Span)
  case If(cond: Expr, ifTrue: Expr, ifFalse: Expr, span: Span)
  case While(cond: Expr, body: Expr, span: Span)
  case For(iterator: Identifier, iterable: Expr, body: Expr, span: Span)
  case Invalid(span: Span)

  def span: Span