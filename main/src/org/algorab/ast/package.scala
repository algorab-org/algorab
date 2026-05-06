/**
 * Shared AST definitions for the Algorab compiler.
 *
 * This package provides types that are used by both the untyped (`untpd`) and
 * typed (`tpd`) abstract syntax tree representations.
 *
 * @see [[org.algorab.ast.untpd]] for the untyped AST produced by the parser.
 * @see [[org.algorab.ast.tpd]] for the typed AST produced by the type-checker.
 */
package org.algorab.ast

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*

/**
 * A non-blank string that represents an Algorab identifier (variable name, type name, etc.).
 *
 * Built on top of Iron's refined type machinery to statically prevent accidental
 * construction of empty or blank identifiers.  Use `Identifier("foo")` for literals
 * that are known at compile time to be valid, and `Identifier.assume` when the value
 * has already been validated externally.
 *
 * Three `CanEqual` instances are provided so that identifiers can be compared with
 * each other and with plain `String` values in `==` / `match` expressions.
 */
type Identifier = Identifier.T
object Identifier extends RefinedType[String, Not[Blank]]:

  given CanEqual[Identifier, Identifier] = CanEqual.derived
  given CanEqual[Identifier, String] = CanEqual.derived
  given CanEqual[String, Identifier] = CanEqual.derived
