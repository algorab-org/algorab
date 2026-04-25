/**
 * Refined types used throughout the compiler back-end.
 *
 * Both [[ParamCount]] and [[InstrPosition]] are non-negative integers refined via the
 * Iron library so that invalid values (negative argument counts, negative code offsets)
 * are caught at construction time rather than producing silent errors at runtime.
 */
package org.algorab.compiler

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*
import scala.annotation.targetName

/** A non-negative function-call argument count. */
type ParamCount = ParamCount.T
object ParamCount extends RefinedType[Int, Positive0]

/**
 * A non-negative absolute position (index) within the flat instruction array.
 *
 * Arithmetic extension methods allow offset calculations without losing the
 * refined-type guarantee.
 */
type InstrPosition = InstrPosition.T
object InstrPosition extends RefinedType[Int, Positive0]:

  extension (value: InstrPosition)
    /**
     * Adds a plain `Int` offset to this position.
     *
     * @param position a non-negative integer offset
     * @return the resulting instruction position
     */
    @targetName("plusInt")
    def +(position: Int): InstrPosition = InstrPosition.assume(value.value + position)

    /**
     * Adds another [[InstrPosition]] to this position.
     *
     * @param position the offset to add
     * @return the resulting instruction position
     */
    def +(position: InstrPosition): InstrPosition = value + position.value
