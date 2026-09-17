package org.algorab.util

/**
 * An error occurring during console input.
 */
enum ConsoleError:

  /**
   * The input is not a valid integer.
   *
   * @param got the invalid input
   */
  case InvalidInt(got: String)

  /**
   * The input is not a valid floating-point number.
   *
   * @param got the invalid input
   */
  case InvalidFloat(got: String)

  /**
   * The end of the input was reached.
   */
  case EndOfInput
