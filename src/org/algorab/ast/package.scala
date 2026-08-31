package org.algorab.ast

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*

/**
 * A reference relative name. Does not have to be unique since shadowing is allowed.
 */
type Identifier = Identifier.T
object Identifier extends RefinedSubtype[String, Match["[a-zA-Z][a-zA-Z0-9]*"]]

/**
 * An unique symbol identifier.
 */
type SymbolId = SymbolId.T
object SymbolId extends RefinedType[Int, GreaterEqual[-1]]:

  extension (x: SymbolId)

    /**
     * Add a positive integer to this symbol id.
     *
     * @param y a positive integer
     * @return this symbol id's value + [[y]]
     */
    def +(y: Int :| Positive0): SymbolId = SymbolId.assume(x.value + y)

    /**
     * Like [[scala.math.max]] but for [[SymbolId]].
     *
     * @param y the other symbol id to compare
     * @return the biggest symbol id between this one and [[y]]
     */
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

  val UnitTerm: SymbolId = SymbolId(8)
  val ToFloatTerm: SymbolId = SymbolId(9)
  val PrintLnTerm: SymbolId = SymbolId(10)
  val ReadIntTerm: SymbolId = SymbolId(11)
  val ReadFloatTerm: SymbolId = SymbolId(12)

  given CanEqual[SymbolId, SymbolId] = CanEqual.derived

/**
 * An unique scope identifier.
 */
type ScopeId = ScopeId.T
object ScopeId extends RefinedType[Int, GreaterEqual[-1]]:

  extension (x: ScopeId)
    /**
     * Add a positive integer to this scope id.
     *
     * @param y a positive integer
     * @return this scope id's value + [[y]]
     */
    def +(y: Int :| Positive0): ScopeId = ScopeId.assume(x.value + y)

  val Invalid: ScopeId = ScopeId(-1)
  val Root: ScopeId = ScopeId.Root
