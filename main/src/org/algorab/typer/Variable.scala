/** Metadata record for a single variable managed by the type-checker.
  *
  * A [[Variable]] is created in [[TypeContext]] the first time a name is declared, and then
  * progressively enriched as the typer learns more about it (e.g. after the initialiser has
  * been typed, `initialized` is set to `true`; if the variable is captured by a nested
  * mutable closure, `boxxed` is set to `true`).
  *
  * The [[VariableId]] assigned at declaration time serves as the stable key used by
  * [[org.algorab.ast.tpd.Expr]] nodes and the [[org.algorab.compiler.Compiler]] to look up
  * this record without re-traversing the scope chain.
  */
package org.algorab.typer

import kyo.Absent
import kyo.Maybe
import org.algorab.ast.Identifier
import org.algorab.ast.tpd.Type

/** Metadata for a single variable in the type context.
  *
  * @param localName   the source-level name of the variable
  * @param tpe         the variable's resolved type (may be [[Type.Inferred]] temporarily)
  * @param mutable     `true` if the variable was declared with `mut val`
  * @param boxxed      `true` if the variable must be stored in a [[org.algorab.compiler.Value.VBox]]
  *                    because it is a mutable variable captured by a closure
  * @param initialized `true` once the variable's initialiser has been type-checked;
  *                    forward references to uninitialized variables produce a
  *                    [[TypeFailure.IllegalForwardReference]]
  * @param field       `true` if the variable is a class field (stored on `this` rather than
  *                    in the local scope); set automatically by [[TypeContext.declareVariable]]
  * @param functionId  the internal name of the function this variable refers to, if it is a
  *                    function binding; used to detect cyclic closures
  * @param classId     the internal name of the class this variable refers to, if it is a
  *                    class binding
  */
case class Variable(
    localName: Identifier,
    tpe: Type,
    mutable: Boolean,
    boxxed: Boolean,
    initialized: Boolean,
    field: Boolean = false,
    functionId: Maybe[Identifier] = Absent,
    classId: Maybe[Identifier] = Absent
):

  /** `true` when this variable holds a reference to a function definition. */
  def isFunDef: Boolean = functionId.isDefined

  /** `true` when this variable holds a reference to a class definition. */
  def isClassDef: Boolean = classId.isDefined
