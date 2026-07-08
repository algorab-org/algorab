package org.algorab.ast

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*

type Identifier = Identifier.T
object Identifier extends RefinedType[String, Match["[a-zA-Z][a-zA-Z0-9]*"]]