/**
 * Typed expression AST nodes, produced by [[org.algorab.typer.Typer]].
 *
 * Every node carries an `exprType: Type` field that records the type assigned by the
 * type-checker.  This information is consumed by [[org.algorab.compiler.Compiler]] to
 * emit correct bytecode (e.g. choosing between `Return` and inserting a `Push(VUnit)`
 * epilogue for `Unit`-typed functions).
 *
 * Variable references are resolved to a stable [[org.algorab.typer.VariableId]] so that
 * the compiler can look up variable metadata (boxed/field status) without rescanning
 * the scope chain.
 */
package org.algorab.ast.tpd

import kyo.Chunk
import org.algorab.ast.Identifier
import org.algorab.ast.tpd.Type
import org.algorab.typer.VariableId

/**
 * A typed Algorab expression.
 *
 * All cases mirror their counterparts in [[org.algorab.ast.untpd.Expr]] except that:
 *   - Every case carries an `exprType` field.
 *   - Variable-related cases carry a resolved [[VariableId]].
 *   - [[ValDef]] is lowered to an [[Assign]] (declaration is tracked separately in [[Block.declarations]]).
 *   - [[untpd.Expr.FunDef]] / [[untpd.Expr.ClassDef]] become an [[Assign]] to a [[FunRef]] / [[ClassRef]].
 *   - [[untpd.Expr.For]] is fully desugared to a `While` over an index variable.
 *   - [[untpd.Expr.TypeApply]] is erased; type arguments are baked into the surrounding expression type.
 */
enum Expr:
  // Literals

  /**
   * A typed boolean literal.
   *
   * @param value    the boolean value
   * @param exprType always [[Type.Boolean]]
   */
  case LBool(value: Boolean, exprType: Type)

  /**
   * A typed integer literal.
   *
   * @param value    the integer value
   * @param exprType always [[Type.Int]]
   */
  case LInt(value: Int, exprType: Type)

  /**
   * A typed floating-point literal.
   *
   * @param value    the double value
   * @param exprType always [[Type.Float]]
   */
  case LFloat(value: Double, exprType: Type)

  /**
   * A typed character literal.
   *
   * @param value    the character value
   * @param exprType always [[Type.Char]]
   */
  case LChar(value: Char, exprType: Type)

  /**
   * A typed string literal.
   *
   * @param value    the string content (escape sequences already processed)
   * @param exprType always [[Type.String]]
   */
  case LString(value: String, exprType: Type)

  // Logical / comparison operators

  /**
   * Logical negation.
   *
   * @param expr     the boolean operand
   * @param exprType always [[Type.Boolean]]
   */
  case Not(expr: Expr, exprType: Type)

  /**
   * Equality test.
   *
   * @param left     the left-hand operand
   * @param right    the right-hand operand
   * @param exprType always [[Type.Boolean]]
   */
  case Equal(left: Expr, right: Expr, exprType: Type)

  /**
   * Inequality test.
   *
   * @param left     the left-hand operand
   * @param right    the right-hand operand
   * @param exprType always [[Type.Boolean]]
   */
  case NotEqual(left: Expr, right: Expr, exprType: Type)

  /**
   * Strict less-than comparison.
   *
   * @param exprType always [[Type.Boolean]]
   */
  case Less(left: Expr, right: Expr, exprType: Type)

  /**
   * Less-than-or-equal comparison.
   *
   * @param exprType always [[Type.Boolean]]
   */
  case LessEqual(left: Expr, right: Expr, exprType: Type)

  /**
   * Strict greater-than comparison.
   *
   * @param exprType always [[Type.Boolean]]
   */
  case Greater(left: Expr, right: Expr, exprType: Type)

  /**
   * Greater-than-or-equal comparison.
   *
   * @param exprType always [[Type.Boolean]]
   */
  case GreaterEqual(left: Expr, right: Expr, exprType: Type)

  // Arithmetic operators

  /**
   * Unary arithmetic negation.
   *
   * @param expr     the numeric operand
   * @param exprType [[Type.Int]] or [[Type.Float]] depending on `expr`
   */
  case Minus(expr: Expr, exprType: Type)

  /**
   * Addition or string concatenation.
   *
   * @param exprType [[Type.Int]], [[Type.Float]], or [[Type.String]]
   */
  case Add(left: Expr, right: Expr, exprType: Type)

  /**
   * Subtraction.
   *
   * @param exprType [[Type.Int]] or [[Type.Float]]
   */
  case Sub(left: Expr, right: Expr, exprType: Type)

  /**
   * Multiplication.
   *
   * @param exprType [[Type.Int]] or [[Type.Float]]
   */
  case Mul(left: Expr, right: Expr, exprType: Type)

  /**
   * Floating-point division (always returns [[Type.Float]]).
   *
   * @param exprType always [[Type.Float]]
   */
  case Div(left: Expr, right: Expr, exprType: Type)

  /**
   * Truncating integer division (always returns [[Type.Int]]).
   *
   * @param exprType always [[Type.Int]]
   */
  case IntDiv(left: Expr, right: Expr, exprType: Type)

  /**
   * Modulo remainder.
   *
   * @param exprType [[Type.Int]] or [[Type.Float]]
   */
  case Mod(left: Expr, right: Expr, exprType: Type)

  /**
   * Short-circuit logical AND (compiled with a conditional jump).
   *
   * @param exprType always [[Type.Boolean]]
   */
  case And(left: Expr, right: Expr, exprType: Type)

  /**
   * Short-circuit logical OR (compiled with a conditional jump).
   *
   * @param exprType always [[Type.Boolean]]
   */
  case Or(left: Expr, right: Expr, exprType: Type)

  // Variables / bindings

  /**
   * A resolved variable reference.
   *
   * @param id       the stable identifier for this variable in the [[org.algorab.typer.TypeContext]]
   * @param name     the source-level name (retained for diagnostics and bytecode labels)
   * @param exprType the variable's declared type
   */
  case VarCall(id: VariableId, name: Identifier, exprType: Type)

  /**
   * A variable declaration with an initialiser (lowered from `untpd.ValDef`).
   *
   * The [[Assign]] produced during type-checking wraps the initialiser; this node
   * additionally records the declared type for use by the compiler.
   *
   * @param id       the variable's stable identifier
   * @param name     the source-level name
   * @param tpe      the resolved declared type
   * @param expr     the initialiser expression
   * @param exprType always [[Type.Unit]]
   */
  case ValDef(id: VariableId, name: Identifier, tpe: Type, expr: Expr, exprType: Type)

  /**
   * Assignment to a variable.
   *
   * Used both for explicit assignments (`name = expr`) and internally for function/class
   * declarations (which are represented as assignments to a [[FunRef]] / [[ClassRef]]).
   *
   * @param id       the variable's stable identifier
   * @param name     the source-level name
   * @param expr     the new value expression
   * @param exprType always [[Type.Unit]]
   */
  case Assign(id: VariableId, name: Identifier, expr: Expr, exprType: Type)

  // Calls and selections

  /**
   * A function or constructor call.
   *
   * @param expr     the callee (a [[VarCall]], [[FunRef]], or [[ClassRef]])
   * @param args     the typed argument expressions
   * @param exprType the return type of the callee
   */
  case Apply(expr: Expr, args: Chunk[Expr], exprType: Type)

  /**
   * A reference to a compiled function by its internal (unique) name.
   *
   * @param internalName the unique name assigned by the typer (may differ from the source name)
   * @param exprType     the function's type
   */
  case FunRef(internalName: Identifier, exprType: Type)

  /**
   * A reference to a compiled class constructor by its internal (unique) name.
   *
   * @param internalName the unique name assigned by the typer
   * @param exprType     the [[Type.Class]] wrapping the constructor type
   */
  case ClassRef(internalName: Identifier, exprType: Type)

  /**
   * Member (field) selection on an instance.
   *
   * @param id       the stable identifier of the selected field
   * @param expr     the receiver expression
   * @param name     the field name
   * @param exprType the field's resolved type (with generic substitutions applied)
   */
  case Select(id: VariableId, expr: Expr, name: Identifier, exprType: Type)

  // Control flow

  /**
   * A scoped block of expressions.
   *
   * `declarations` lists all variables that must be declared at block entry so that forward
   * references (e.g. mutually recursive functions) work correctly.  `expressions` is the
   * ordered list of statements; the block's value is that of the last expression.
   *
   * @param declarations variables declared at the start of this scope, as `(id, name)` pairs
   * @param expressions  the ordered sub-expressions forming the block body
   * @param exprType     the type of the last expression (or [[Type.Unit]] for an empty block)
   */
  case Block(declarations: Chunk[(VariableId, Identifier)], expressions: Chunk[Expr], exprType: Type)

  /**
   * A typed conditional expression.
   *
   * @param cond     the boolean condition
   * @param ifTrue   the expression for the `true` branch
   * @param ifFalse  the expression for the `false` branch
   * @param exprType the unified type of both branches
   */
  case If(cond: Expr, ifTrue: Expr, ifFalse: Expr, exprType: Type)

  /**
   * A `while` loop.
   *
   * @param cond     the boolean condition
   * @param body     the loop body
   * @param exprType always [[Type.Unit]]
   */
  case While(cond: Expr, body: Expr, exprType: Type)

  /** Returns the static type of this expression. */
  def exprType: Type

  /**
   * Returns a copy of this expression node with the type field replaced by `tpe`.
   *
   * Every case is matched exhaustively and its other fields are preserved unchanged.
   *
   * @param tpe the new type to attach
   * @return a structurally identical expression carrying `tpe`
   */
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
