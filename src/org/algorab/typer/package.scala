package org.algorab.typer

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*

type VariableId = VariableId.T
object VariableId extends RefinedType[Int, Positive0]