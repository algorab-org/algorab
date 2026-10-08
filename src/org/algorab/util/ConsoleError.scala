package org.algorab.util

/**
 * An error occurring during console input.
 */
enum ConsoleError derives CanEqual:

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

  def message: String = this match
    case InvalidInt(got) => s"Invalid Int.\nGot: $got"
    case InvalidFloat(got) => s"Invalid Float.\nGot: $got"
    case EndOfInput => "No remaining input."
  