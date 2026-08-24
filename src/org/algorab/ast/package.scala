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
    def max(y: SymbolId): SymbolId = SymbolId.assume(math.max(x.value, y.value))

  val Invalid: SymbolId = SymbolId(-1)
  val Root: SymbolId = SymbolId(0)
  
  val AnyType: SymbolId = SymbolId(1)
  val UnitType: SymbolId = SymbolId(2)
  val BooleanType: SymbolId = SymbolId(3)
  val IntType: SymbolId = SymbolId(4)
  val FloatType: SymbolId = SymbolId(5)
  val CharType: SymbolId = SymbolId(6)
  val StringType: SymbolId = SymbolId(7)

  val ToFloatTerm: SymbolId = SymbolId(8)
  val PrintLnTerm: SymbolId = SymbolId(9)
  val ReadIntTerm: SymbolId = SymbolId(10)
  val ReadFloatTerm: SymbolId = SymbolId(11)

  given CanEqual[SymbolId, SymbolId] = CanEqual.derived

type ScopeId = ScopeId.T
object ScopeId extends RefinedType[Int, GreaterEqual[-1]]:

  extension (x: ScopeId)
    def +(y: Int :| Positive0): ScopeId = ScopeId.assume(x.value + y)

  val Invalid: ScopeId = ScopeId(-1)
  val Root: ScopeId = ScopeId.Root