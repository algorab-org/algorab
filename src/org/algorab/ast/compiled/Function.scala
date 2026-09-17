package org.algorab.ast.compiled

/**
 * A compiled function.
 *
 * @param body the function's instructions
 */
case class Function(body: Array[Instruction])
