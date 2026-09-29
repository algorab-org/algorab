package org.algorab.ast.typed

import org.algorab.ast.SymbolId
import org.algorab.util.SourcePosition

/**
 * An Algorab expression.
 */
enum Expr:

  /**
   * A boolean literal.
   *
   * @param value the literal's value
   * @param tpe this expression's type
   * @param position the source position of this expression
   */
  case LBool(value: Boolean, tpe: Type, position: SourcePosition)

  /**
   * An integer literal.
   *
   * @param value the literal's value
   * @param tpe this expression's type
   * @param position the source position of this expression
   */
  case LInt(value: Int, tpe: Type, position: SourcePosition)

  /**
   * A float literal.
   *
   * @param value the literal's value
   * @param tpe this expression's type
   * @param position the source position of this expression
   */
  case LFloat(value: Double, tpe: Type, position: SourcePosition)

  /**
   * A character literal.
   *
   * @param value the literal's value
   * @param tpe this expression's type
   * @param position the source position of this expression
   */
  case LChar(value: Char, tpe: Type, position: SourcePosition)

  /**
   * A string literal.
   *
   * @param value the literal's value
   * @param tpe this expression's type
   * @param position the source position of this expression
   */
  case LString(value: String, tpe: Type, position: SourcePosition)

  /**
   * A boolean not.
   *
   * @param expr the expression to invert
   * @param tpe this expression's type
   * @param position the source position of this expression
   */
  case Not(expr: Expr, tpe: Type, position: SourcePosition)

  /**
   * An equality test.
   *
   * @param left the LHS
   * @param right the RHS
   * @param tpe this expression's type
   * @param position the source position of this expression
   */
  case Equal(left: Expr, right: Expr, tpe: Type, position: SourcePosition)

  /**
   * An non-equality test.
   *
   * @param left the LHS
   * @param right the RHS
   * @param tpe this expression's type
   * @param position the source position of this expression
   */
  case NotEqual(left: Expr, right: Expr, tpe: Type, position: SourcePosition)

  /**
   * A numeric inferiority test.
   *
   * @param left the LHS
   * @param right the RHS
   * @param tpe this expression's type
   * @param position the source position of this expression
   */
  case Less(left: Expr, right: Expr, tpe: Type, position: SourcePosition)

  /**
   * A numeric inferiority or equality test.
   *
   * @param left the LHS
   * @param right the RHS
   * @param tpe this expression's type
   * @param position the source position of this expression
   */
  case LessEqual(left: Expr, right: Expr, tpe: Type, position: SourcePosition)

  /**
   * A numeric superiority test.
   *
   * @param left the LHS
   * @param right the RHS
   * @param tpe this expression's type
   * @param position the source position of this expression
   */
  case Greater(left: Expr, right: Expr, tpe: Type, position: SourcePosition)

  /**
   * A numeric superiority and equality test.
   *
   * @param left the LHS
   * @param right the RHS
   * @param tpe this expression's type
   * @param position the source position of this expression
   */
  case GreaterEqual(left: Expr, right: Expr, tpe: Type, position: SourcePosition)

  /**
   * A numeric plus. Usually does nothing.
   *
   * @param expr the prefixed expression
   * @param tpe this expression's type
   * @param position the source position of this expression
   */
  case Plus(expr: Expr, tpe: Type, position: SourcePosition)

  /**
   * A numeric negation.
   *
   * @param expr the prefixed expression to negate
   * @param tpe this expression's type
   * @param position the source position of this expression
   */
  case Minus(expr: Expr, tpe: Type, position: SourcePosition)

  /**
   * An addition.
   *
   * @param left the LHS
   * @param right the RHS
   * @param tpe this expression's type
   * @param position the source position of this expression
   */
  case Add(left: Expr, right: Expr, tpe: Type, position: SourcePosition)

  /**
   * A subtraction.
   *
   * @param left the LHS
   * @param right the RHS
   * @param tpe this expression's type
   * @param position the source position of this expression
   */
  case Sub(left: Expr, right: Expr, tpe: Type, position: SourcePosition)

  /**
   * A multiplication.
   *
   * @param left the LHS
   * @param right the RHS
   * @param tpe this expression's type
   * @param position the source position of this expression
   */
  case Mul(left: Expr, right: Expr, tpe: Type, position: SourcePosition)

  /**
   * A decimal division.
   *
   * @param left the LHS
   * @param right the RHS
   * @param tpe this expression's type
   * @param position the source position of this expression
   */
  case Div(left: Expr, right: Expr, tpe: Type, position: SourcePosition)

  /**
   * An integer division.
   *
   * @param left the LHS
   * @param right the RHS
   * @param tpe this expression's type
   * @param position the source position of this expression
   */
  case IntDiv(left: Expr, right: Expr, tpe: Type, position: SourcePosition)

  /**
   * A modulo aka the remainder of an euclidean division.
   *
   * @param left the LHS
   * @param right the RHS
   * @param tpe this expression's type
   * @param position the source position of this expression
   */
  case Mod(left: Expr, right: Expr, tpe: Type, position: SourcePosition)

  /**
   * A boolean and.
   *
   * @param left the LHS
   * @param right the RHS
   * @param tpe this expression's type
   * @param position the source position of this expression
   */
  case And(left: Expr, right: Expr, tpe: Type, position: SourcePosition)

  /**
   * An boolean or.
   *
   * @param left the LHS
   * @param right the RHS
   * @param tpe this expression's type
   * @param position the source position of this expression
   */
  case Or(left: Expr, right: Expr, tpe: Type, position: SourcePosition)

  /**
   * A variable call.
   *
   * @param symbol the unique id of the variable to call
   * @param tpe this expression's type
   * @param position the source position of this expression
   */
  case VarCall(symbol: SymbolId, tpe: Type, position: SourcePosition)

  /**
   * A value assignation to a variable.
   *
   * @param symbol the unique id of the variable to assign to
   * @param expr the expression whose value is to be assigned
   * @param tpe this expression's type
   * @param position the source position of this expression
   */
  case Assign(symbol: SymbolId, expr: Expr, tpe: Type, position: SourcePosition)

  /**
   * A function application.
   *
   * @param expr the function to apply
   * @param args the arguments to pass to the function
   * @param tpe this expression's type
   * @param position the source position of this expression
   */
  case Apply(expr: Expr, args: List[Expr], tpe: Type, position: SourcePosition)

  /**
   * A block of one or more statements.
   *
   * @param statements this block's statements
   * @param tpe this expression's type
   * @param position the source position of this expression
   */
  case Block(statements: List[Statement], tpe: Type, position: SourcePosition)

  /**
   * An if-else expression.
   *
   * @param cond the condition to test
   * @param ifTrue the body to evaluate if the condition is true
   * @param ifFalse the body to evaluate if the condition is false
   * @param tpe this expression's type
   * @param position the source position of this expression
   */
  case If(cond: Expr, ifTrue: Expr, ifFalse: Expr, tpe: Type, position: SourcePosition)

  /**
   * A while loop.
   *
   * @param cond the condition to test for each iteration
   * @param body the body to evaluate at each iteration
   * @param tpe this expression's type
   * @param position the source position of this expression
   */
  case While(cond: Expr, body: Expr, tpe: Type, position: SourcePosition)

  /**
   * A for loop.
   *
   * @param iterator the iterator's unique id
   * @param iterable the collection to iterate on
   * @param body the body to evaluate at each iteration
   * @param tpe this expression's type
   * @param position the source position of this expression
   */
  case For(iterator: SymbolId, iterable: Expr, body: Expr, tpe: Type, position: SourcePosition)

  /**
   * An invalid expression.
   *
   * @param tpe this expression's type
   * @param position the source position of this expression
   */
  case Invalid(tpe: Type, position: SourcePosition)

  /**
   * This expression's type.
   */
  def tpe: Type

  /**
   * The source position of this expression.
   */
  def position: SourcePosition

  def withType(tpe: Type): Expr = this match
    case LBool(value, _, position)                  => LBool(value, tpe, position)
    case LInt(value, _, position)                   => LInt(value, tpe, position)
    case LFloat(value, _, position)                 => LFloat(value, tpe, position)
    case LChar(value, _, position)                  => LChar(value, tpe, position)
    case LString(value, _, position)                => LString(value, tpe, position)
    case Not(expr, _, position)                     => Not(expr, tpe, position)
    case Equal(left, right, _, position)            => Equal(left, right, tpe, position)
    case NotEqual(left, right, _, position)         => NotEqual(left, right, tpe, position)
    case Less(left, right, _, position)             => Less(left, right, tpe, position)
    case LessEqual(left, right, _, position)        => LessEqual(left, right, tpe, position)
    case Greater(left, right, _, position)          => Greater(left, right, tpe, position)
    case GreaterEqual(left, right, _, position)     => GreaterEqual(left, right, tpe, position)
    case Plus(expr, _, position)                    => Plus(expr, tpe, position)
    case Minus(expr, _, position)                   => Minus(expr, tpe, position)
    case Add(left, right, _, position)              => Add(left, right, tpe, position)
    case Sub(left, right, _, position)              => Sub(left, right, tpe, position)
    case Mul(left, right, _, position)              => Mul(left, right, tpe, position)
    case Div(left, right, _, position)              => Div(left, right, tpe, position)
    case IntDiv(left, right, _, position)           => IntDiv(left, right, tpe, position)
    case Mod(left, right, _, position)              => Mod(left, right, tpe, position)
    case And(left, right, _, position)              => And(left, right, tpe, position)
    case Or(left, right, _, position)               => Or(left, right, tpe, position)
    case VarCall(symbol, _, position)               => VarCall(symbol, tpe, position)
    case Assign(symbol, expr, _, position)          => Assign(symbol, expr, tpe, position)
    case Apply(expr, args, _, position)             => Apply(expr, args, tpe, position)
    case Block(statements, _, position)             => Block(statements, tpe, position)
    case If(cond, ifTrue, ifFalse, _, position)     => If(cond, ifTrue, ifFalse, tpe, position)
    case While(cond, body, _, position)             => While(cond, body, tpe, position)
    case For(iterator, iterable, body, _, position) => For(iterator, iterable, body, tpe, position)
    case Invalid(_, position)                       => Invalid(tpe, position)

object Expr:

  object ToFloat:

    def apply(expr: Expr): Expr = Apply(
      VarCall(SymbolId.ToFloatTerm, Type.Function(List(Type.Int), Type.Float), expr.position),
      List(expr),
      Type.Float,
      expr.position
    )

    def unapply(toFloat: Expr): Option[(Expr, SourcePosition)] = toFloat match
      case Apply(VarCall(SymbolId.ToFloatTerm, _, _), List(expr), _, position) => Some((expr, position))
      case _                                                                   => None
