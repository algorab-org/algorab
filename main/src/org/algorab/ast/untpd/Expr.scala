/**
 * Untyped expression AST nodes produced by the parser.
 *
 * Nodes in this tree carry no type information; every case only stores the
 * structural information that is directly visible in Algorab source code.
 * The type-checker (see [[org.algorab.typer.Typer]]) consumes these nodes
 * and produces the annotated counterparts in [[org.algorab.ast.tpd]].
 */
package org.algorab.ast.untpd

import kyo.Chunk
import org.algorab.ast.Identifier

/**
 * An untyped Algorab expression.
 *
 * The enum is organised into the following groups:
 *   - '''Literals''': `LBool`, `LInt`, `LFloat`, `LChar`, `LString`
 *   - '''Logical operators''': `Not`, `And`, `Or`
 *   - '''Comparison operators''': `Equal`, `NotEqual`, `Less`, `LessEqual`, `Greater`, `GreaterEqual`
 *   - '''Arithmetic operators''': `Plus`, `Minus`, `Add`, `Sub`, `Mul`, `Div`, `IntDiv`, `Mod`
 *   - '''Variables and definitions''': `VarCall`, `ValDef`, `Assign`
 *   - '''Calls and selections''': `Apply`, `TypeApply`, `Select`
 *   - '''Top-level definitions''': `FunDef`, `ClassDef`
 *   - '''Control flow''': `Block`, `If`, `While`, `For`
 */
enum Expr:
  // Literals

  /** A boolean literal (`true` or `false`). */
  case LBool(value: Boolean)

  /** An integer literal (e.g. `42`). */
  case LInt(value: Int)

  /** A floating-point literal (e.g. `3.14` or `1e10`). */
  case LFloat(value: Double)

  /** A character literal (e.g. `'a'`). */
  case LChar(value: Char)

  /** A string literal (e.g. `"hello"`). */
  case LString(value: String)

  // Logical / comparison operators

  /** Logical negation (`not expr`). */
  case Not(expr: Expr)

  /** Equality comparison (`left == right`). */
  case Equal(left: Expr, right: Expr)

  /** Inequality comparison (`left != right`). */
  case NotEqual(left: Expr, right: Expr)

  /** Strict less-than comparison (`left < right`). */
  case Less(left: Expr, right: Expr)

  /** Less-than-or-equal comparison (`left <= right`). */
  case LessEqual(left: Expr, right: Expr)

  /** Strict greater-than comparison (`left > right`). */
  case Greater(left: Expr, right: Expr)

  /** Greater-than-or-equal comparison (`left >= right`). */
  case GreaterEqual(left: Expr, right: Expr)

  // Arithmetic operators

  /** Unary plus (`+expr`). Desugared to the identity during type-checking. */
  case Plus(expr: Expr)

  /** Unary negation (`-expr`). */
  case Minus(expr: Expr)

  /** Addition or string concatenation (`left + right`). */
  case Add(left: Expr, right: Expr)

  /** Subtraction (`left - right`). */
  case Sub(left: Expr, right: Expr)

  /** Multiplication (`left * right`). */
  case Mul(left: Expr, right: Expr)

  /** Floating-point division (`left / right`). Always yields a `Float`. */
  case Div(left: Expr, right: Expr)

  /** Integer (truncating) division (`left // right`). */
  case IntDiv(left: Expr, right: Expr)

  /** Modulo remainder (`left % right`). */
  case Mod(left: Expr, right: Expr)

  /** Short-circuit logical AND (`left and right`). */
  case And(left: Expr, right: Expr)

  /** Short-circuit logical OR (`left or right`). */
  case Or(left: Expr, right: Expr)

  // Variables / bindings

  /** Reference to a variable by name. */
  case VarCall(name: Identifier)

  /**
   * Immutable (`val`) or mutable (`mut val`) variable binding.
   *
   * @param name    the variable name
   * @param tpe     the declared type, or [[Type.Inferred]] if omitted
   * @param expr    the initialiser expression
   * @param mutable `true` when the `mut` keyword precedes `val`
   */
  case ValDef(name: Identifier, tpe: Type, expr: Expr, mutable: Boolean)

  /**
   * Assignment to a mutable variable (`name = expr`).
   *
   * @param name the target variable name
   * @param expr the new value expression
   */
  case Assign(name: Identifier, expr: Expr)

  // Calls and selections

  /**
   * Function or constructor application (`expr(args...)`).
   *
   * @param expr the callee expression (usually a [[VarCall]] or [[Select]])
   * @param args the positional argument expressions
   */
  case Apply(expr: Expr, args: Chunk[Expr])

  /**
   * Explicit type-argument application (`expr[types...]`).
   *
   * @param expr  the polymorphic expression being specialised
   * @param types the explicit type arguments
   */
  case TypeApply(expr: Expr, types: Chunk[Type])

  // Top-level definitions

  /**
   * A named function definition (`def name[typeParams](params): retType = body`).
   *
   * @param name       the function's declared name
   * @param typeParams names of any generic type parameters (empty for monomorphic functions)
   * @param params     parameter names paired with their declared types
   * @param retType    the declared return type, or [[Type.Inferred]]
   * @param body       the single expression that forms the function body
   */
  case FunDef(name: Identifier, typeParams: Chunk[Identifier], params: Chunk[(Identifier, Type)], retType: Type, body: Expr)

  /**
   * A class definition (`class Name[typeParams](params) = body`).
   *
   * @param name       the class name
   * @param typeParams names of any generic type parameters
   * @param params     constructor parameter names paired with their declared types
   * @param body       the expressions that form the class body (field initialisers, method defs, etc.)
   */
  case ClassDef(name: Identifier, typeParams: Chunk[Identifier], params: Chunk[(Identifier, Type)], body: Chunk[Expr])

  /**
   * Member selection on an object (`expr.name`).
   *
   * @param expr the receiver expression
   * @param name the member name to select
   */
  case Select(expr: Expr, name: Identifier)

  // Control flow

  /**
   * A sequence of expressions evaluated in order; the block's value is that of the last expression.
   *
   * @param expressions the ordered list of sub-expressions
   */
  case Block(expressions: Chunk[Expr])

  /**
   * Conditional expression (`if cond then ifTrue else ifFalse`).
   *
   * When no `else` branch is present the parser inserts `VarCall("Unit")`.
   *
   * @param cond    the boolean condition
   * @param ifTrue  the expression evaluated when `cond` is `true`
   * @param ifFalse the expression evaluated when `cond` is `false`
   */
  case If(cond: Expr, ifTrue: Expr, ifFalse: Expr)

  /**
   * A `while` loop (`while cond do body`).
   *
   * @param cond the loop condition; re-evaluated before each iteration
   * @param body the loop body
   */
  case While(cond: Expr, body: Expr)

  /**
   * A `for` loop that iterates over an array (`for iterator in iterable do body`).
   *
   * Desugared to a `while` loop over an index variable during type-checking.
   *
   * @param iterator the name of the loop variable
   * @param iterable the array-typed expression to iterate over
   * @param body     the loop body, which may reference `iterator`
   */
  case For(iterator: Identifier, iterable: Expr, body: Expr)
