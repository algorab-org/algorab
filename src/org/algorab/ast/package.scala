package org.algorab.ast

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*

type Identifier = Identifier.T
object Identifier extends RefinedSubtype[String, Match["[a-zA-Z][a-zA-Z0-9]*"]]

type QualifiedName = QualifiedName.T
object QualifiedName extends RefinedType[String, Pure]

type SymbolId = SymbolId.T
object SymbolId extends RefinedType[Int, GreaterEqual[-1]]:

  extension (x: SymbolId)
    def +(y: Int :| Positive0): SymbolId = SymbolId.assume(x.value + y)

  val Invalid: SymbolId = SymbolId(-1)