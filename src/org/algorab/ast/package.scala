package org.algorab.ast

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*

type Identifier = Identifier.T
object Identifier extends RefinedType[String, Not[Blank]]:

  given CanEqual[Identifier, String] = CanEqual.derived
  given CanEqual[String, Identifier] = CanEqual.derived
