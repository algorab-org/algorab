package org.algorab.ast.resolved

import io.github.iltotore.pureparser.Span
import org.algorab.ast.SymbolId
import org.algorab.ast.Identifier

/**
 * An Algorab expression.
 */
enum Expr:

  /**
   * A boolean literal.
   *
   * @param value the literal's value
   * @param span the source position of this expression
   */
  case LBool(value: Boolean, span: Span)

  /**
   * An integer literal.
   *
   * @param value the literal's value
   * @param span the source position of this expression
   */
  case LInt(value: Int, span: Span)

  /**
   * A float literal.
   *
   * @param value the literal's value
   * @param span the source position of this expression
   */
  case LFloat(value: Double, span: Span)

  /**
   * A character literal.
   *
   * @param value the literal's value
   * @param span the source position of this expression
   */
  case LChar(value: Char, span: Span)

  /**
   * A string literal.
   *
   * @param value the literal's value
   * @param span the source position of this expression
   */
  case LString(value: String, span: Span)

  /**
   * A boolean not.
   *
   * @param expr the expression to invert
   * @param span the source position of this expression
   */
  case Not(expr: Expr, span: Span)

  /**
   * An equality test.
   *
   * @param left the LHS
   * @param right the RHS
   * @param span the source position of this expression
   */
  case Equal(left: Expr, right: Expr, span: Span)

  /**
   * An non-equality test.
   *
   * @param left the LHS
   * @param right the RHS
   * @param span the source position of this expression
   */
  case NotEqual(left: Expr, right: Expr, span: Span)

  /**
   * A numeric inferiority test.
   *
   * @param left the LHS
   * @param right the RHS
   * @param span the source position of this expression
   */
  case Less(left: Expr, right: Expr, span: Span)

  /**
   * A numeric inferiority or equality test.
   *
   * @param left the LHS
   * @param right the RHS
   * @param span the source position of this expression
   */
  case LessEqual(left: Expr, right: Expr, span: Span)

  /**
   * A numeric superiority test.
   *
   * @param left the LHS
   * @param right the RHS
   * @param span the source position of this expression
   */
  case Greater(left: Expr, right: Expr, span: Span)

  /**
   * A numeric superiority and equality test.
   *
   * @param left the LHS
   * @param right the RHS
   * @param span the source position of this expression
   */
  case GreaterEqual(left: Expr, right: Expr, span: Span)

  /**
   * A numeric plus. Usually does nothing.
   *
   * @param expr the prefixed expression
   * @param span the source position of this expression
   */
  case Plus(expr: Expr, span: Span)

  /**
   * A numeric negation.
   *
   * @param expr the prefixed expression to negate
   * @param span the source position of this expression
   */
  case Minus(expr: Expr, span: Span)

  /**
   * An addition.
   *
   * @param left the LHS
   * @param right the RHS
   * @param span the source position of this expression
   */
  case Add(left: Expr, right: Expr, span: Span)

  /**
   * A subtraction.
   *
   * @param left the LHS
   * @param right the RHS
   * @param span the source position of this expression
   */
  case Sub(left: Expr, right: Expr, span: Span)

  /**
   * A multiplication.
   *
   * @param left the LHS
   * @param right the RHS
   * @param span the source position of this expression
   */
  case Mul(left: Expr, right: Expr, span: Span)

  /**
   * A decimal division.
   *
   * @param left the LHS
   * @param right the RHS
   * @param span the source position of this expression
   */
  case Div(left: Expr, right: Expr, span: Span)

  /**
   * An integer division.
   *
   * @param left the LHS
   * @param right the RHS
   * @param span the source position of this expression
   */
  case IntDiv(left: Expr, right: Expr, span: Span)

  /**
   * A modulo aka the remainder of an euclidean division.
   *
   * @param left the LHS
   * @param right the RHS
   * @param span the source position of this expression
   */
  case Mod(left: Expr, right: Expr, span: Span)

  /**
   * A boolean and.
   *
   * @param left the LHS
   * @param right the RHS
   * @param span the source position of this expression
   */
  case And(left: Expr, right: Expr, span: Span)

  /**
   * An boolean or.
   *
   * @param left the LHS
   * @param right the RHS
   * @param span the source position of this expression
   */
  case Or(left: Expr, right: Expr, span: Span)

  /**
   * A variable call.
   *
   * @param symbol the unique id of the variable to call
   * @param span the source position of this expression
   */
  case VarCall(symbol: SymbolId, span: Span)

  /**
   * A value assignation to a variable.
   *
   * @param symbol the unique id of the variable to assign to
   * @param expr the expression whose value is to be assigned
   * @param span the source position of this expression
   */
  case Assign(symbol: SymbolId, expr: Expr, span: Span)

  case Select(expr: Expr, member: Identifier, span: Span)

  /**
   * A function application.
   *
   * @param expr the function to apply
   * @param args the arguments to pass to the function
   * @param span the source position of this expression
   */
  case Apply(expr: Expr, args: List[Expr], span: Span)

  /**
   * A block of one or more statements.
   *
   * @param statements this block's statements
   * @param span the source position of this expression
   */
  case Block(statements: List[Statement], span: Span)

  /**
   * An if-else expression.
   *
   * @param cond the condition to test
   * @param ifTrue the body to evaluate if the condition is true
   * @param ifFalse the body to evaluate if the condition is false
   * @param span the source position of this expression
   */
  case If(cond: Expr, ifTrue: Expr, ifFalse: Expr, span: Span)

  /**
   * A while loop.
   *
   * @param cond the condition to test for each iteration
   * @param body the body to evaluate at each iteration
   * @param span the source position of this expression
   */
  case While(cond: Expr, body: Expr, span: Span)

  /**
   * A for loop.
   *
   * @param iterator the iterator's unique id
   * @param iterable the collection to iterate on
   * @param body the body to evaluate at each iteration
   * @param span the source position of this expression
   */
  case For(iterator: SymbolId, iterable: Expr, body: Expr, span: Span)

  /**
   * An invalid expression.
   *
   * @param span the source position of this expression
   */
  case Invalid(span: Span)

  /**
   * The source position of this expression.
   */
  def span: Span
