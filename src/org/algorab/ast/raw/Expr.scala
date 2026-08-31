package org.algorab.ast.raw

import io.github.iltotore.pureparser.Span
import org.algorab.ast.Identifier
import org.algorab.ast.raw.Statement

/**
 * An Algorab expression.
 */
enum Expr:

  /**
   * A boolean literal.
   *
   * @param value the literal's value
   * @param span the source position of this declaration
   */
  case LBool(value: Boolean, span: Span)

  /**
   * An integer literal.
   *
   * @param value the literal's value
   * @param span the source position of this declaration
   */
  case LInt(value: Int, span: Span)

  /**
   * A float literal.
   *
   * @param value the literal's value
   * @param span the source position of this declaration
   */
  case LFloat(value: Double, span: Span)

  /**
   * A character literal.
   *
   * @param value the literal's value
   * @param span the source position of this declaration
   */
  case LChar(value: Char, span: Span)

  /**
   * A string literal.
   *
   * @param value the literal's value
   * @param span the source position of this declaration
   */
  case LString(value: String, span: Span)

  /**
   * A boolean not.
   *
   * @param expr the expression to invert
   * @param span the source position of this declaration
   */
  case Not(expr: Expr, span: Span)

  /**
   * An equality test.
   *
   * @param left the LHS
   * @param right the RHS
   * @param span the source position of this declaration
   */
  case Equal(left: Expr, right: Expr, span: Span)

  /**
   * An non-equality test.
   *
   * @param left the LHS
   * @param right the RHS
   * @param span the source position of this declaration
   */
  case NotEqual(left: Expr, right: Expr, span: Span)

  /**
   * A numeric inferiority test.
   *
   * @param left the LHS
   * @param right the RHS
   * @param span the source position of this declaration
   */
  case Less(left: Expr, right: Expr, span: Span)

  /**
   * A numeric inferiority or equality test.
   *
   * @param left the LHS
   * @param right the RHS
   * @param span the source position of this declaration
   */
  case LessEqual(left: Expr, right: Expr, span: Span)

  /**
   * A numeric superiority test.
   *
   * @param left the LHS
   * @param right the RHS
   * @param span the source position of this declaration
   */
  case Greater(left: Expr, right: Expr, span: Span)

  /**
   * A numeric superiority and equality test.
   *
   * @param left the LHS
   * @param right the RHS
   * @param span the source position of this declaration
   */
  case GreaterEqual(left: Expr, right: Expr, span: Span)

  /**
   * A numeric plus. Usually does nothing.
   *
   * @param expr the prefixed expression
   * @param span the source position of this declaration
   */
  case Plus(expr: Expr, span: Span)

  /**
   * A numeric negation.
   *
   * @param expr the prefixed expression to negate
   * @param span the source position of this declaration
   */
  case Minus(expr: Expr, span: Span)

  /**
   * An addition.
   *
   * @param left the LHS
   * @param right the RHS
   * @param span the source position of this declaration
   */
  case Add(left: Expr, right: Expr, span: Span)

  /**
   * A subtraction.
   *
   * @param left the LHS
   * @param right the RHS
   * @param span the source position of this declaration
   */
  case Sub(left: Expr, right: Expr, span: Span)

  /**
   * A multiplication.
   *
   * @param left the LHS
   * @param right the RHS
   * @param span the source position of this declaration
   */
  case Mul(left: Expr, right: Expr, span: Span)

  /**
   * A decimal division.
   *
   * @param left the LHS
   * @param right the RHS
   * @param span the source position of this declaration
   */
  case Div(left: Expr, right: Expr, span: Span)

  /**
   * An integer division.
   *
   * @param left the LHS
   * @param right the RHS
   * @param span the source position of this declaration
   */
  case IntDiv(left: Expr, right: Expr, span: Span)

  /**
   * A modulo aka the remainder of an euclidean division.
   *
   * @param left the LHS
   * @param right the RHS
   * @param span the source position of this declaration
   */
  case Mod(left: Expr, right: Expr, span: Span)

  /**
   * A boolean and.
   *
   * @param left the LHS
   * @param right the RHS
   * @param span the source position of this declaration
   */
  case And(left: Expr, right: Expr, span: Span)

  /**
   * An boolean or.
   *
   * @param left the LHS
   * @param right the RHS
   * @param span the source position of this declaration
   */
  case Or(left: Expr, right: Expr, span: Span)

  /**
   * A variable call.
   *
   * @param name the name of the variable to call
   * @param span the source position of this declaration
   */
  case VarCall(name: Identifier, span: Span)

  /**
   * A value assignation to a variable.
   *
   * @param name the name of the variable to assign to
   * @param expr the expression whose value is to be assigned
   * @param span the source position of this declaration
   */
  case Assign(name: Identifier, expr: Expr, span: Span)

  /**
   * A function application.
   *
   * @param expr the function to apply
   * @param args the arguments to pass to the function
   * @param span the source position of this declaration
   */
  case Apply(expr: Expr, args: List[Expr], span: Span)

  /**
   * A block of one or more statements.
   *
   * @param statements this block's statements
   * @param span the source position of this declaration
   */
  case Block(statements: List[Statement], span: Span)

  /**
   * An if-else expression.
   *
   * @param cond the condition to test
   * @param ifTrue the body to evaluate if the condition is true
   * @param ifFalse the body to evaluate if the condition is false
   * @param span the source position of this declaration
   */
  case If(cond: Expr, ifTrue: Expr, ifFalse: Expr, span: Span)

  /**
   * A while loop.
   *
   * @param cond the condition to test for each iteration
   * @param body the body to evaluate at each iteration
   * @param span the source position of this declaration
   */
  case While(cond: Expr, body: Expr, span: Span)

  /**
   * A for loop.
   *
   * @param iterator the iterator name
   * @param iterable the collection to iterate on
   * @param body the body to evaluate at each iteration
   * @param span the source position of this declaration
   */
  case For(iterator: Identifier, iterable: Expr, body: Expr, span: Span)

  /**
   * An invalid expression.
   *
   * @param span the source position of this declaration
   */
  case Invalid(span: Span)

  /**
   * The source position of this expression.
   */
  def span: Span
