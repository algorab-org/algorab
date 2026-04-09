/** A single lexical scope in the type-checker's scope chain.
  *
  * [[TypeScope]] tracks which names are visible at any given point in the source.
  * Three variants model the three binding contexts in Algorab:
  *   - `Block`    – an ordinary expression block (`{ … }`).
  *   - `Function` – the body of a `def`, which additionally tracks the set of names
  *                  captured from outer scopes.
  *   - `Class`    – the body of a `class`, which stops capture propagation (outer names
  *                  accessed from inside a class become field accesses, not closures).
  *
  * All variants store a `types` map (user-defined type aliases / generics) and a `variables`
  * map (source names → [[VariableId]]).
  */
package org.algorab.typer

import kyo.*
import org.algorab.ast.Identifier
import org.algorab.ast.tpd.Type

/** A lexical scope used during type-checking.
  *
  * Scopes are maintained as a stack inside [[TypeContext]]; the head of the stack is the
  * innermost scope.
  */
enum TypeScope:
  /** An ordinary block scope, created for `if`/`while`/`for` bodies and explicit blocks.
    *
    * @param types     type bindings visible in this scope (generic params, type aliases)
    * @param variables variable bindings visible in this scope
    */
  case Block(types: Map[Identifier, Type], variables: Map[Identifier, VariableId])

  /** A function scope, used inside `def` bodies.
    *
    * In addition to the standard bindings, tracks which names from outer scopes are
    * referenced inside this function (the `captures` set).
    *
    * @param id        the [[VariableId]] of the function itself (used to update its metadata
    *                  after the body is typed)
    * @param types     type bindings visible in this scope
    * @param variables variable bindings visible in this scope
    * @param captures  names from outer scopes that have been referenced inside this function
    */
  case Function(id: VariableId, types: Map[Identifier, Type], variables: Map[Identifier, VariableId], captures: Set[Identifier])

  /** A class scope, used inside `class` bodies.
    *
    * Class scopes are opaque to capture propagation: if code inside a method references
    * a name that is defined outside the class, the typer does ''not'' propagate the capture
    * upward; instead it treats the reference as a field access via `this`.
    *
    * @param id        the [[VariableId]] of the class constructor variable
    * @param types     type bindings visible in this scope (includes the class's own name
    *                  mapped to its instance type)
    * @param variables variable bindings visible in this scope (includes `this`)
    */
  case Class(id: VariableId, types: Map[Identifier, Type], variables: Map[Identifier, VariableId], captures: Set[Identifier])

  /** The type bindings map for this scope. */
  def types: Map[Identifier, Type]

  /** The variable bindings map for this scope. */
  def variables: Map[Identifier, VariableId]

  /** Returns the type bound to `name` in this scope, if any.
    *
    * @param name the type name to look up
    * @return `Some(tpe)` if found, `None` otherwise
    */
  def getType(name: Identifier): Option[Type] =
    types.get(name)

  /** Returns a copy of this scope with `name` bound to `tpe`.
    *
    * @param name the type name to bind
    * @param tpe  the type value to associate with `name`
    */
  def withType(name: Identifier, tpe: Type): TypeScope = this match
    case Block(types, variables) => Block(types.updated(name, tpe), variables)
    case Function(id, types, variables, captures) =>
      Function(id, types.updated(name, tpe), variables, captures)
    case Class(id, types, variables, captures) =>
      Class(id, types.updated(name, tpe), variables, captures)

  /** Returns the [[VariableId]] bound to `name` in this scope, if any.
    *
    * @param name the variable name to look up
    * @return `Some(id)` if found, `None` otherwise
    */
  def getVariable(name: Identifier): Option[VariableId] =
    variables.get(name)

  /** Returns a copy of this scope with `name` bound to `variable`.
    *
    * @param name     the variable name to bind
    * @param variable the [[VariableId]] to associate with `name`
    */
  def withVariable(name: Identifier, variable: VariableId): TypeScope = this match
    case Block(types, variables) => Block(types, variables.updated(name, variable))
    case Function(id, types, variables, captures) =>
      Function(id, types, variables.updated(name, variable), captures)
    case Class(id, types, variables, captures) =>
      Class(id, types, variables.updated(name, variable), captures)

  /** `true` iff this is a [[Class]] scope.
    *
    * Used by [[TypeContext.declareVariable]] to mark variables declared inside a class as fields.
    */
  def isClassScope: Boolean = this.isInstanceOf[TypeScope.Class]
