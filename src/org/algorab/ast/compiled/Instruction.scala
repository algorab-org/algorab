package org.algorab.ast.compiled

import org.algorab.ast.InstructionPosition
import org.algorab.ast.ParamCount
import org.algorab.ast.SymbolId
import org.algorab.ast.Value
import org.algorab.util.SourcePosition

/**
 * An instruction in the compiled representation of an Algorab program.
 */
enum Instruction:

  /**
   * Push a value onto the stack.
   *
   * @param value the value to push
   * @param position the source position of this instruction
   */
  case Push(value: Value, position: SourcePosition)

  /**
   * Negate a boolean value.
   *
   * @param position the source position of this instruction
   */
  case Not(position: SourcePosition)

  /**
   * Test two values for equality.
   *
   * @param position the source position of this instruction
   */
  case Equal(position: SourcePosition)

  /**
   * Test two values for inequality.
   *
   * @param position the source position of this instruction
   */
  case NotEqual(position: SourcePosition)

  /**
   * Convert an integer to a float.
   *
   * @param position the source position of this instruction
   */
  case ToFloat(position: SourcePosition)

  /**
   * Compare two integers for inferiority.
   *
   * @param position the source position of this instruction
   */
  case LessInt(position: SourcePosition)

  /**
   * Compare two integers for inferiority or equality.
   *
   * @param position the source position of this instruction
   */
  case LessEqualInt(position: SourcePosition)

  /**
   * Compare two integers for superiority.
   *
   * @param position the source position of this instruction
   */
  case GreaterInt(position: SourcePosition)

  /**
   * Compare two integers for superiority or equality.
   *
   * @param position the source position of this instruction
   */
  case GreaterEqualInt(position: SourcePosition)

  /**
   * Negate an integer.
   *
   * @param position the source position of this instruction
   */
  case MinusInt(position: SourcePosition)

  /**
   * Add two integers.
   *
   * @param position the source position of this instruction
   */
  case AddInt(position: SourcePosition)

  /**
   * Subtract two integers.
   *
   * @param position the source position of this instruction
   */
  case SubInt(position: SourcePosition)

  /**
   * Multiply two integers.
   *
   * @param position the source position of this instruction
   */
  case MulInt(position: SourcePosition)

  /**
   * Divide two integers.
   *
   * @param position the source position of this instruction
   */
  case DivInt(position: SourcePosition)

  /**
   * Perform integer division on two integers.
   *
   * @param position the source position of this instruction
   */
  case IntDivInt(position: SourcePosition)

  /**
   * Compute the remainder of two integer values.
   *
   * @param position the source position of this instruction
   */
  case ModInt(position: SourcePosition)

  /**
   * Compare two floats for inferiority.
   *
   * @param position the source position of this instruction
   */
  case LessFloat(position: SourcePosition)

  /**
   * Compare two floats for inferiority or equality.
   *
   * @param position the source position of this instruction
   */
  case LessEqualFloat(position: SourcePosition)

  /**
   * Compare two floats for superiority.
   *
   * @param position the source position of this instruction
   */
  case GreaterFloat(position: SourcePosition)

  /**
   * Compare two floats for superiority or equality.
   *
   * @param position the source position of this instruction
   */
  case GreaterEqualFloat(position: SourcePosition)

  /**
   * Negate a float.
   *
   * @param position the source position of this instruction
   */
  case MinusFloat(position: SourcePosition)

  /**
   * Add two floats.
   *
   * @param position the source position of this instruction
   */
  case AddFloat(position: SourcePosition)

  /**
   * Subtract two floats.
   *
   * @param position the source position of this instruction
   */
  case SubFloat(position: SourcePosition)

  /**
   * Multiply two floats.
   *
   * @param position the source position of this instruction
   */
  case MulFloat(position: SourcePosition)

  /**
   * Divide two floats.
   *
   * @param position the source position of this instruction
   */
  case DivFloat(position: SourcePosition)

  /**
   * Perform integer division on two floats.
   *
   * @param position the source position of this instruction
   */
  case IntDivFloat(position: SourcePosition)

  /**
   * Compute the remainder of two float values.
   *
   * @param position the source position of this instruction
   */
  case ModFloat(position: SourcePosition)

  /**
   * Store a value in a local variable.
   *
   * @param symbol the unique id of the variable
   * @param position the source position of this instruction
   */
  case Store(symbol: SymbolId, position: SourcePosition)

  /**
   * Store a value in a global variable.
   *
   * @param symbol the unique id of the variable
   * @param position the source position of this instruction
   */
  case StoreGlobal(symbol: SymbolId, position: SourcePosition)

  /**
   * Load a value from a local variable.
   *
   * @param symbol the unique id of the variable
   * @param position the source position of this instruction
   */
  case Load(symbol: SymbolId, position: SourcePosition)

  /**
   * Load a value from a global variable.
   *
   * @param symbol the unique id of the variable
   * @param position the source position of this instruction
   */
  case LoadGlobal(symbol: SymbolId, position: SourcePosition)

  /**
   * Apply a function to arguments on the stack.
   *
   * @param paramCount the number of parameters to pass
   * @param position the source position of this instruction
   */
  case Apply(paramCount: ParamCount, position: SourcePosition)

  /**
   * Jump to an instruction position.
   *
   * @param to the instruction position to jump to
   * @param position the source position of this instruction
   */
  case Jump(to: InstructionPosition, position: SourcePosition)

  /**
   * Jump to an instruction position if the top of the stack is false.
   *
   * @param to the instruction position to jump to
   * @param position the source position of this instruction
   */
  case JumpIfFalse(to: InstructionPosition, position: SourcePosition)

  /**
   * Return from the current function.
   *
   * @param position the source position of this instruction
   */
  case Return(position: SourcePosition)

  /**
   * The source position of this instruction.
   */
  def position: SourcePosition
