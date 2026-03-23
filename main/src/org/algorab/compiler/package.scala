package org.algorab.compiler

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*
import scala.annotation.targetName

type ParamCount = ParamCount.T
object ParamCount extends RefinedType[Int, Positive0]

type InstrPosition = InstrPosition.T
object InstrPosition extends RefinedType[Int, Positive0]:

  extension (value: InstrPosition)
    @targetName("plusInt")
    def +(position: Int): InstrPosition = InstrPosition.assume(value.value + position)
    def +(position: InstrPosition): InstrPosition = value + position.value
