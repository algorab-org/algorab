/** Type-checker representation of a compiled function definition.
  *
  * A [[FunctionDef]] is stored in [[TypeContext.functions]] after the typer has fully
  * processed a `def` declaration.  It carries all the information that the
  * [[org.algorab.compiler.Compiler]] needs to emit the function's bytecode and register it
  * in the runtime function table.
  */
package org.algorab.typer

import kyo.Chunk
import org.algorab.ast.Identifier
import org.algorab.ast.tpd.Expr
import org.algorab.ast.tpd.Type

/** A typed function definition stored in [[TypeContext]].
  *
  * @param displayName the source-level function name, used for diagnostics and at runtime
  * @param params      the parameter names in declaration order
  * @param captures    the set of [[VariableId]]s of variables closed over by this function;
  *                    populated by the capture-analysis pass in [[TypeContext.popFunction]]
  * @param body        the fully type-annotated body expression
  * @param varId       the [[VariableId]] of the variable that holds this function's value
  *                    (the binding created when the `def` was declared)
  */
case class FunctionDef(
    displayName: Identifier,
    params: Chunk[Identifier],
    captures: Set[VariableId],
    body: Expr,
    varId: VariableId
)
