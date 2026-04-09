/** Typer-internal refined types.
  *
  * [[VariableId]] is the stable, integer-indexed key used to look up [[Variable]] metadata
  * in the flat `variables: Chunk[Variable]` array inside [[TypeContext]].  Using a refined
  * type (Iron `Positive0`) instead of a plain `Int` prevents negative indices at construction
  * time.
  */
package org.algorab.typer

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*

/** A non-negative index into the `TypeContext.variables` array. */
type VariableId = VariableId.T
object VariableId extends RefinedType[Int, Positive0]:

  given CanEqual[VariableId, VariableId] = CanEqual.derived
