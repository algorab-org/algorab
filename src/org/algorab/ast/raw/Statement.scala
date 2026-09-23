package org.algorab.ast.raw
import org.algorab.ast.raw.Definition
import org.algorab.ast.raw.Expr

/**
 * A statement, whether an expression or a definition.
 */
type Statement = Expr | Definition | Import
