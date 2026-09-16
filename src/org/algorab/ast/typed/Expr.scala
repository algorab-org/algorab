package org.algorab.ast.typed

import io.github.iltotore.pureparser.Span
import org.algorab.ast.SymbolId

/**
 * An Algorab expression.
 */
enum Expr:

  /**
   * A boolean literal.
   *
   * @param value the literal's value
   * @param tpe this expression's type
   * @param span the source position of this expression
   */
  case LBool(value: Boolean, tpe: Type, span: Span)

  /**
   * An integer literal.
   *
   * @param value the literal's value
   * @param tpe this expression's type
   * @param span the source position of this expression
   */
  case LInt(value: Int, tpe: Type, span: Span)

  /**
   * A float literal.
   *
   * @param value the literal's value
   * @param tpe this expression's type
   * @param span the source position of this expression
   */
  case LFloat(value: Double, tpe: Type, span: Span)

  /**
   * A character literal.
   *
   * @param value the literal's value
   * @param tpe this expression's type
   * @param span the source position of this expression
   */
  case LChar(value: Char, tpe: Type, span: Span)

  /**
   * A string literal.
   *
   * @param value the literal's value
   * @param tpe this expression's type
   * @param span the source position of this expression
   */
  case LString(value: String, tpe: Type, span: Span)

  /**
   * A boolean not.
   *
   * @param expr the expression to invert
   * @param tpe this expression's type
   * @param span the source position of this expression
   */
  case Not(expr: Expr, tpe: Type, span: Span)

  /**
   * An equality test.
   *
   * @param left the LHS
   * @param right the RHS
   * @param tpe this expression's type
   * @param span the source position of this expression
   */
  case Equal(left: Expr, right: Expr, tpe: Type, span: Span)

  /**
   * An non-equality test.
   *
   * @param left the LHS
   * @param right the RHS
   * @param tpe this expression's type
   * @param span the source position of this expression
   */
  case NotEqual(left: Expr, right: Expr, tpe: Type, span: Span)

  /**
   * A numeric inferiority test.
   *
   * @param left the LHS
   * @param right the RHS
   * @param tpe this expression's type
   * @param span the source position of this expression
   */
  case Less(left: Expr, right: Expr, tpe: Type, span: Span)

  /**
   * A numeric inferiority or equality test.
   *
   * @param left the LHS
   * @param right the RHS
   * @param tpe this expression's type
   * @param span the source position of this expression
   */
  case LessEqual(left: Expr, right: Expr, tpe: Type, span: Span)

  /**
   * A numeric superiority test.
   *
   * @param left the LHS
   * @param right the RHS
   * @param tpe this expression's type
   * @param span the source position of this expression
   */
  case Greater(left: Expr, right: Expr, tpe: Type, span: Span)

  /**
   * A numeric superiority and equality test.
   *
   * @param left the LHS
   * @param right the RHS
   * @param tpe this expression's type
   * @param span the source position of this expression
   */
  case GreaterEqual(left: Expr, right: Expr, tpe: Type, span: Span)

  /**
   * A numeric plus. Usually does nothing.
   *
   * @param expr the prefixed expression
   * @param tpe this expression's type
   * @param span the source position of this expression
   */
  case Plus(expr: Expr, tpe: Type, span: Span)

  /**
   * A numeric negation.
   *
   * @param expr the prefixed expression to negate
   * @param tpe this expression's type
   * @param span the source position of this expression
   */
  case Minus(expr: Expr, tpe: Type, span: Span)

  /**
   * An addition.
   *
   * @param left the LHS
   * @param right the RHS
   * @param tpe this expression's type
   * @param span the source position of this expression
   */
  case Add(left: Expr, right: Expr, tpe: Type, span: Span)

  /**
   * A subtraction.
   *
   * @param left the LHS
   * @param right the RHS
   * @param tpe this expression's type
   * @param span the source position of this expression
   */
  case Sub(left: Expr, right: Expr, tpe: Type, span: Span)

  /**
   * A multiplication.
   *
   * @param left the LHS
   * @param right the RHS
   * @param tpe this expression's type
   * @param span the source position of this expression
   */
  case Mul(left: Expr, right: Expr, tpe: Type, span: Span)

  /**
   * A decimal division.
   *
   * @param left the LHS
   * @param right the RHS
   * @param tpe this expression's type
   * @param span the source position of this expression
   */
  case Div(left: Expr, right: Expr, tpe: Type, span: Span)

  /**
   * An integer division.
   *
   * @param left the LHS
   * @param right the RHS
   * @param tpe this expression's type
   * @param span the source position of this expression
   */
  case IntDiv(left: Expr, right: Expr, tpe: Type, span: Span)

  /**
   * A modulo aka the remainder of an euclidean division.
   *
   * @param left the LHS
   * @param right the RHS
   * @param tpe this expression's type
   * @param span the source position of this expression
   */
  case Mod(left: Expr, right: Expr, tpe: Type, span: Span)

  /**
   * A boolean and.
   *
   * @param left the LHS
   * @param right the RHS
   * @param tpe this expression's type
   * @param span the source position of this expression
   */
  case And(left: Expr, right: Expr, tpe: Type, span: Span)

  /**
   * An boolean or.
   *
   * @param left the LHS
   * @param right the RHS
   * @param tpe this expression's type
   * @param span the source position of this expression
   */
  case Or(left: Expr, right: Expr, tpe: Type, span: Span)

  /**
   * A variable call.
   *
   * @param symbol the unique id of the variable to call
   * @param tpe this expression's type
   * @param span the source position of this expression
   */
  case VarCall(symbol: SymbolId, tpe: Type, span: Span)

  /**
   * A value assignation to a variable.
   *
   * @param symbol the unique id of the variable to assign to
   * @param expr the expression whose value is to be assigned
   * @param tpe this expression's type
   * @param span the source position of this expression
   */
  case Assign(symbol: SymbolId, expr: Expr, tpe: Type, span: Span)

  /**
   * A function application.
   *
   * @param expr the function to apply
   * @param args the arguments to pass to the function
   * @param tpe this expression's type
   * @param span the source position of this expression
   */
  case Apply(expr: Expr, args: List[Expr], tpe: Type, span: Span)

  /**
   * A block of one or more statements.
   *
   * @param statements this block's statements
   * @param tpe this expression's type
   * @param span the source position of this expression
   */
  case Block(statements: List[Statement], tpe: Type, span: Span)

  /**
   * An if-else expression.
   *
   * @param cond the condition to test
   * @param ifTrue the body to evaluate if the condition is true
   * @param ifFalse the body to evaluate if the condition is false
   * @param tpe this expression's type
   * @param span the source position of this expression
   */
  case If(cond: Expr, ifTrue: Expr, ifFalse: Expr, tpe: Type, span: Span)

  /**
   * A while loop.
   *
   * @param cond the condition to test for each iteration
   * @param body the body to evaluate at each iteration
   * @param tpe this expression's type
   * @param span the source position of this expression
   */
  case While(cond: Expr, body: Expr, tpe: Type, span: Span)

  /**
   * A for loop.
   *
   * @param iterator the iterator's unique id
   * @param iterable the collection to iterate on
   * @param body the body to evaluate at each iteration
   * @param tpe this expression's type
   * @param span the source position of this expression
   */
  case For(iterator: SymbolId, iterable: Expr, body: Expr, tpe: Type, span: Span)

  /**
   * An invalid expression.
   *
   * @param tpe this expression's type
   * @param span the source position of this expression
   */
  case Invalid(tpe: Type, span: Span)

  /**
   * This expression's type.
   */
  def tpe: Type

  /**
   * The source position of this expression.
   */
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

  object ToFloat:

    def apply(expr: Expr): Expr = Apply(
      VarCall(SymbolId.ToFloatTerm, Type.Function(List(Type.Int), Type.Float), expr.span),
      List(expr),
      Type.Float,
      expr.span
    )

    def unapply(toFloat: Expr): Option[(Expr, Span)] = toFloat match
      case Apply(VarCall(SymbolId.ToFloatTerm, _, _), List(expr), _, span) => Some((expr, span))
      case _                                                               => None
