/** Type-checker representation of a compiled class definition.
  *
  * A [[ClassTypeDef]] is stored in [[TypeContext.classes]] after the typer has fully
  * processed a `class` declaration.  It carries all information needed by
  * [[org.algorab.compiler.Compiler.compileClass]] to emit the constructor bytecode.
  */
package org.algorab.typer

import kyo.Chunk
import org.algorab.ast.Identifier
import org.algorab.ast.tpd.Expr

/** A typed class definition stored in [[TypeContext]].
  *
  * @param displayName  the source-level class name, used for diagnostics and at runtime
  * @param declarations a map from field name to the [[VariableId]] of the corresponding
  *                     field variable; includes all `val`/`mut val` members declared in
  *                     the class body
  * @param parameters   the constructor parameter names in declaration order
  * @param init         the typed body expressions that initialise the class instance
  *                     (field initialisers, method definitions, etc.)
  * @param varId        the [[VariableId]] of the variable that holds the class constructor
  *                     value (the binding created when the `class` was declared)
  */
case class ClassTypeDef(
    displayName: Identifier,
    declarations: Map[Identifier, VariableId],
    parameters: Chunk[Identifier],
    captures: Set[VariableId],
    init: Chunk[Expr],
    varId: VariableId
)
