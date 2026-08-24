package org.algorab.ast.typed

import io.github.iltotore.pureparser.Span
import org.algorab.ast.SymbolId

enum Expr:
  case LBool(value: Boolean, tpe: Type, span: Span)
  case LInt(value: Int, tpe: Type, span: Span)
  case LFloat(value: Double, tpe: Type, span: Span)
  case LChar(value: Char, tpe: Type, span: Span)
  case LString(value: String, tpe: Type, span: Span)
  case Not(expr: Expr, tpe: Type, span: Span)
  case Equal(left: Expr, right: Expr, tpe: Type, span: Span)
  case NotEqual(left: Expr, right: Expr, tpe: Type, span: Span)
  case Less(left: Expr, right: Expr, tpe: Type, span: Span)
  case LessEqual(left: Expr, right: Expr, tpe: Type, span: Span)
  case Greater(left: Expr, right: Expr, tpe: Type, span: Span)
  case GreaterEqual(left: Expr, right: Expr, tpe: Type, span: Span)
  case Plus(expr: Expr, tpe: Type, span: Span)
  case Minus(expr: Expr, tpe: Type, span: Span)
  case Add(left: Expr, right: Expr, tpe: Type, span: Span)
  case Sub(left: Expr, right: Expr, tpe: Type, span: Span)
  case Mul(left: Expr, right: Expr, tpe: Type, span: Span)
  case Div(left: Expr, right: Expr, tpe: Type, span: Span)
  case IntDiv(left: Expr, right: Expr, tpe: Type, span: Span)
  case Mod(left: Expr, right: Expr, tpe: Type, span: Span)
  case And(left: Expr, right: Expr, tpe: Type, span: Span)
  case Or(left: Expr, right: Expr, tpe: Type, span: Span)
  case VarCall(symbol: SymbolId, tpe: Type, span: Span)
  case Assign(symbol: SymbolId, expr: Expr, tpe: Type, span: Span)
  case Apply(expr: Expr, args: List[Expr], tpe: Type, span: Span)
  case Block(statements: List[Statement], tpe: Type, span: Span)
  case If(cond: Expr, ifTrue: Expr, ifFalse: Expr, tpe: Type, span: Span)
  case While(cond: Expr, body: Expr, tpe: Type, span: Span)
  case For(iterator: SymbolId, iterable: Expr, body: Expr, tpe: Type, span: Span)
  case Invalid(tpe: Type, span: Span)

  def tpe: Type

  def span: Span

  def withType(tpe: Type): Expr = this match
    case LBool(value, _, span)                  => LBool(value, tpe, span)
    case LInt(value, _, span)                   => LInt(value, tpe, span)
    case LFloat(value, _, span)                 => LFloat(value, tpe, span)
    case LChar(value, _, span)                  => LChar(value, tpe, span)
    case LString(value, _, span)                => LString(value, tpe, span)
    case Not(expr, _, span)                     => Not(expr, tpe, span)
    case Equal(left, right, _, span)            => Equal(left, right, tpe, span)
    case NotEqual(left, right, _, span)         => NotEqual(left, right, tpe, span)
    case Less(left, right, _, span)             => Less(left, right, tpe, span)
    case LessEqual(left, right, _, span)        => LessEqual(left, right, tpe, span)
    case Greater(left, right, _, span)          => Greater(left, right, tpe, span)
    case GreaterEqual(left, right, _, span)     => GreaterEqual(left, right, tpe, span)
    case Plus(expr, _, span)                    => Plus(expr, tpe, span)
    case Minus(expr, _, span)                   => Minus(expr, tpe, span)
    case Add(left, right, _, span)              => Add(left, right, tpe, span)
    case Sub(left, right, _, span)              => Sub(left, right, tpe, span)
    case Mul(left, right, _, span)              => Mul(left, right, tpe, span)
    case Div(left, right, _, span)              => Div(left, right, tpe, span)
    case IntDiv(left, right, _, span)           => IntDiv(left, right, tpe, span)
    case Mod(left, right, _, span)              => Mod(left, right, tpe, span)
    case And(left, right, _, span)              => And(left, right, tpe, span)
    case Or(left, right, _, span)               => Or(left, right, tpe, span)
    case VarCall(symbol, _, span)               => VarCall(symbol, tpe, span)
    case Assign(symbol, expr, _, span)          => Assign(symbol, expr, tpe, span)
    case Apply(expr, args, _, span)             => Apply(expr, args, tpe, span)
    case Block(statements, _, span)             => Block(statements, tpe, span)
    case If(cond, ifTrue, ifFalse, _, span)     => If(cond, ifTrue, ifFalse, tpe, span)
    case While(cond, body, _, span)             => While(cond, body, tpe, span)
    case For(iterator, iterable, body, _, span) => For(iterator, iterable, body, tpe, span)
    case Invalid(_, span)                       => Invalid(tpe, span)

object Expr:

  def ToFloat(expr: Expr): Expr = Apply(
    VarCall(SymbolId.ToFloatTerm, Type.Function(List(Type.Int), Type.Float), expr.span),
    List(expr),
    Type.Float,
    expr.span
  )