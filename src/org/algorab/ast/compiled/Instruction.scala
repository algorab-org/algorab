package org.algorab.ast.compiled

import io.github.iltotore.pureparser.Span
import org.algorab.ast.InstructionPosition
import org.algorab.ast.ParamCount
import org.algorab.ast.SymbolId
import org.algorab.ast.Value

/**
 * An instruction in the compiled representation of an Algorab program.
 */
enum Instruction:

  /**
   * Push a value onto the stack.
   *
   * @param value the value to push
   * @param span the source position of this instruction
   */
  case Push(value: Value, span: Span)

  /**
   * Negate a boolean value.
   *
   * @param span the source position of this instruction
   */
  case Not(span: Span)

  /**
   * Test two values for equality.
   *
   * @param span the source position of this instruction
   */
  case Equal(span: Span)

  /**
   * Test two values for inequality.
   *
   * @param span the source position of this instruction
   */
  case NotEqual(span: Span)

  /**
   * Convert an integer to a float.
   *
   * @param span the source position of this instruction
   */
  case ToFloat(span: Span)

  /**
   * Compare two integers for inferiority.
   *
   * @param span the source position of this instruction
   */
  case LessInt(span: Span)

  /**
   * Compare two integers for inferiority or equality.
   *
   * @param span the source position of this instruction
   */
  case LessEqualInt(span: Span)

  /**
   * Compare two integers for superiority.
   *
   * @param span the source position of this instruction
   */
  case GreaterInt(span: Span)

  /**
   * Compare two integers for superiority or equality.
   *
   * @param span the source position of this instruction
   */
  case GreaterEqualInt(span: Span)

  /**
   * Negate an integer.
   *
   * @param span the source position of this instruction
   */
  case MinusInt(span: Span)

  /**
   * Add two integers.
   *
   * @param span the source position of this instruction
   */
  case AddInt(span: Span)

  /**
   * Subtract two integers.
   *
   * @param span the source position of this instruction
   */
  case SubInt(span: Span)

  /**
   * Multiply two integers.
   *
   * @param span the source position of this instruction
   */
  case MulInt(span: Span)

  /**
   * Divide two integers.
   *
   * @param span the source position of this instruction
   */
  case DivInt(span: Span)

  /**
   * Perform integer division on two integers.
   *
   * @param span the source position of this instruction
   */
  case IntDivInt(span: Span)

  /**
   * Compute the remainder of two integer values.
   *
   * @param span the source position of this instruction
   */
  case ModInt(span: Span)

  /**
   * Compare two floats for inferiority.
   *
   * @param span the source position of this instruction
   */
  case LessFloat(span: Span)

  /**
   * Compare two floats for inferiority or equality.
   *
   * @param span the source position of this instruction
   */
  case LessEqualFloat(span: Span)

  /**
   * Compare two floats for superiority.
   *
   * @param span the source position of this instruction
   */
  case GreaterFloat(span: Span)

  /**
   * Compare two floats for superiority or equality.
   *
   * @param span the source position of this instruction
   */
  case GreaterEqualFloat(span: Span)

  /**
   * Negate a float.
   *
   * @param span the source position of this instruction
   */
  case MinusFloat(span: Span)

  /**
   * Add two floats.
   *
   * @param span the source position of this instruction
   */
  case AddFloat(span: Span)

  /**
   * Subtract two floats.
   *
   * @param span the source position of this instruction
   */
  case SubFloat(span: Span)

  /**
   * Multiply two floats.
   *
   * @param span the source position of this instruction
   */
  case MulFloat(span: Span)

  /**
   * Divide two floats.
   *
   * @param span the source position of this instruction
   */
  case DivFloat(span: Span)

  /**
   * Perform integer division on two floats.
   *
   * @param span the source position of this instruction
   */
  case IntDivFloat(span: Span)

  /**
   * Compute the remainder of two float values.
   *
   * @param span the source position of this instruction
   */
  case ModFloat(span: Span)

  /**
   * Store a value in a local variable.
   *
   * @param symbol the unique id of the variable
   * @param span the source position of this instruction
   */
  case Store(symbol: SymbolId, span: Span)

  /**
   * Store a value in a global variable.
   *
   * @param symbol the unique id of the variable
   * @param span the source position of this instruction
   */
  case StoreGlobal(symbol: SymbolId, span: Span)

  /**
   * Load a value from a local variable.
   *
   * @param symbol the unique id of the variable
   * @param span the source position of this instruction
   */
  case Load(symbol: SymbolId, span: Span)

  /**
   * Load a value from a global variable.
   *
   * @param symbol the unique id of the variable
   * @param span the source position of this instruction
   */
  case LoadGlobal(symbol: SymbolId, span: Span)

  /**
   * Apply a function to arguments on the stack.
   *
   * @param paramCount the number of parameters to pass
   * @param span the source position of this instruction
   */
  case Apply(paramCount: ParamCount, span: Span)

  /**
   * Jump to an instruction position.
   *
   * @param to the instruction position to jump to
   * @param span the source position of this instruction
   */
  case Jump(to: InstructionPosition, span: Span)

  /**
   * Jump to an instruction position if the top of the stack is false.
   *
   * @param to the instruction position to jump to
   * @param span the source position of this instruction
   */
  case JumpIfFalse(to: InstructionPosition, span: Span)

  /**
   * Return from the current function.
   *
   * @param span the source position of this instruction
   */
  case Return(span: Span)

  /**
   * The source position of this instruction.
   */
  def span: Span
