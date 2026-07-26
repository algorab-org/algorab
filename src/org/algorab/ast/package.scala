package org.algorab.ast

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*

type Identifier = Identifier.T
object Identifier extends RefinedSubtype[String, Match["[a-zA-Z][a-zA-Z0-9]*"]]

type ResolvedId = ResolvedId.T
object ResolvedId extends RefinedType[String, Pure]