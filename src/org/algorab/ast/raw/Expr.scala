package org.algorab.ast.raw

import org.algorab.ast.Identifier
import org.algorab.ast.raw.Statement
import org.algorab.util.SourcePosition

/**
 * An Algorab expression.
 */
enum Expr:

  /**
   * A boolean literal.
   *
   * @param value the literal's value
   * @param position the source position of this declaration
   */
  case LBool(value: Boolean, position: SourcePosition)

  /**
   * An integer literal.
   *
   * @param value the literal's value
   * @param position the source position of this declaration
   */
  case LInt(value: Int, position: SourcePosition)

  /**
   * A float literal.
   *
   * @param value the literal's value
   * @param position the source position of this declaration
   */
  case LFloat(value: Double, position: SourcePosition)

  /**
   * A character literal.
   *
   * @param value the literal's value
   * @param position the source position of this declaration
   */
  case LChar(value: Char, position: SourcePosition)

  /**
   * A string literal.
   *
   * @param value the literal's value
   * @param position the source position of this declaration
   */
  case LString(value: String, position: SourcePosition)

  /**
   * A boolean not.
   *
   * @param expr the expression to invert
   * @param position the source position of this declaration
   */
  case Not(expr: Expr, position: SourcePosition)

  /**
   * An equality test.
   *
   * @param left the LHS
   * @param right the RHS
   * @param position the source position of this declaration
   */
  case Equal(left: Expr, right: Expr, position: SourcePosition)

  /**
   * An non-equality test.
   *
   * @param left the LHS
   * @param right the RHS
   * @param position the source position of this declaration
   */
  case NotEqual(left: Expr, right: Expr, position: SourcePosition)

  /**
   * A numeric inferiority test.
   *
   * @param left the LHS
   * @param right the RHS
   * @param position the source position of this declaration
   */
  case Less(left: Expr, right: Expr, position: SourcePosition)

  /**
   * A numeric inferiority or equality test.
   *
   * @param left the LHS
   * @param right the RHS
   * @param position the source position of this declaration
   */
  case LessEqual(left: Expr, right: Expr, position: SourcePosition)

  /**
   * A numeric superiority test.
   *
   * @param left the LHS
   * @param right the RHS
   * @param position the source position of this declaration
   */
  case Greater(left: Expr, right: Expr, position: SourcePosition)

  /**
   * A numeric superiority and equality test.
   *
   * @param left the LHS
   * @param right the RHS
   * @param position the source position of this declaration
   */
  case GreaterEqual(left: Expr, right: Expr, position: SourcePosition)

  /**
   * A numeric plus. Usually does nothing.
   *
   * @param expr the prefixed expression
   * @param position the source position of this declaration
   */
  case Plus(expr: Expr, position: SourcePosition)

  /**
   * A numeric negation.
   *
   * @param expr the prefixed expression to negate
   * @param position the source position of this declaration
   */
  case Minus(expr: Expr, position: SourcePosition)

  /**
   * An addition.
   *
   * @param left the LHS
   * @param right the RHS
   * @param position the source position of this declaration
   */
  case Add(left: Expr, right: Expr, position: SourcePosition)

  /**
   * A subtraction.
   *
   * @param left the LHS
   * @param right the RHS
   * @param position the source position of this declaration
   */
  case Sub(left: Expr, right: Expr, position: SourcePosition)

  /**
   * A multiplication.
   *
   * @param left the LHS
   * @param right the RHS
   * @param position the source position of this declaration
   */
  case Mul(left: Expr, right: Expr, position: SourcePosition)

  /**
   * A decimal division.
   *
   * @param left the LHS
   * @param right the RHS
   * @param position the source position of this declaration
   */
  case Div(left: Expr, right: Expr, position: SourcePosition)

  /**
   * An integer division.
   *
   * @param left the LHS
   * @param right the RHS
   * @param position the source position of this declaration
   */
  case IntDiv(left: Expr, right: Expr, position: SourcePosition)

  /**
   * A modulo aka the remainder of an euclidean division.
   *
   * @param left the LHS
   * @param right the RHS
   * @param position the source position of this declaration
   */
  case Mod(left: Expr, right: Expr, position: SourcePosition)

  /**
   * A boolean and.
   *
   * @param left the LHS
   * @param right the RHS
   * @param position the source position of this declaration
   */
  case And(left: Expr, right: Expr, position: SourcePosition)

  /**
   * An boolean or.
   *
   * @param left the LHS
   * @param right the RHS
   * @param position the source position of this declaration
   */
  case Or(left: Expr, right: Expr, position: SourcePosition)

  /**
   * A variable call.
   *
   * @param name the name of the variable to call
   * @param position the source position of this declaration
   */
  case VarCall(name: Identifier, position: SourcePosition)

  /**
   * A value assignation to a variable.
   *
   * @param name the name of the variable to assign to
   * @param expr the expression whose value is to be assigned
   * @param position the source position of this declaration
   */
  case Assign(name: Identifier, expr: Expr, position: SourcePosition)

  /**
   * A reference to the member of an expression.
   * Can be used as a qualified identifier (my.package.foo) or an instance member.
   *
   * @param expr the expression to select from
   * @param member the name of the member to select
   * @param position the source position of this declaration
   */
  case Select(expr: Expr, member: Identifier, position: SourcePosition)

  /**
   * A function application.
   *
   * @param expr the function to apply
   * @param args the arguments to pass to the function
   * @param position the source position of this declaration
   */
  case Apply(expr: Expr, args: List[Expr], position: SourcePosition)

  /**
   * A block of one or more statements.
   *
   * @param statements this block's statements
   * @param position the source position of this declaration
   */
  case Block(statements: List[Statement], position: SourcePosition)

  /**
   * An if-else expression.
   *
   * @param cond the condition to test
   * @param ifTrue the body to evaluate if the condition is true
   * @param ifFalse the body to evaluate if the condition is false
   * @param position the source position of this declaration
   */
  case If(cond: Expr, ifTrue: Expr, ifFalse: Expr, position: SourcePosition)

  /**
   * A while loop.
   *
   * @param cond the condition to test for each iteration
   * @param body the body to evaluate at each iteration
   * @param position the source position of this declaration
   */
  case While(cond: Expr, body: Expr, position: SourcePosition)

  /**
   * A for loop.
   *
   * @param iterator the iterator name
   * @param iterable the collection to iterate on
   * @param body the body to evaluate at each iteration
   * @param position the source position of this declaration
   */
  case For(iterator: Identifier, iterable: Expr, body: Expr, position: SourcePosition)

  /**
   * An invalid expression.
   *
   * @param position the source position of this declaration
   */
  case Invalid(position: SourcePosition)

  /**
   * The source position of this expression.
   */
  def position: SourcePosition
