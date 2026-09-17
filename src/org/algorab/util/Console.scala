package org.algorab.util

import java.io.IOException
import org.algorab.util.ConsoleError
import purelogic.*
import scala.io.StdIn

/**
 * An abstraction over console input and output.
 */
trait Console:

  /**
   * Read a line from the console.
   *
   * @return the input line
   */
  def readLine(): Abort[ConsoleError] ?=> String

  /**
   * Print a line to the console.
   *
   * @param text the text to print
   */
  def println(text: String): Unit

object Console:

  /**
   * A console implementation backed by input and output strings.
   *
   * @param input the input to read from
   * @param output the output produced so far
   */
  case class IO(input: String, output: String):

    /**
     * Read a line from the input.
     *
     * @return the input line and updated console state
     */
    def readLine(): Abort[ConsoleError] ?=> (String, IO) =
      if input.isEmpty then fail(ConsoleError.EndOfInput)
      else
        val endPos = input.indexWhere(c => c == '\n' || c == '\r')
        if endPos == -1 then (input, this.copy(input = ""))
        else if input(endPos) == '\r' && input(endPos + 1) == '\n' then
          (input.take(endPos), this.copy(input = input.drop(endPos + 2)))
        else
          (input.take(endPos), this.copy(input = input.drop(endPos + 1)))

    /**
     * Append text to the output.
     *
     * @param output the text to append
     * @return the updated console state
     */
    def appendOutput(output: String): IO = this.copy(output = this.output + output)

  /**
   * Run a program with an in-memory console.
   *
   * @param program the program to run
   * @return the program result
   */
  def apply[A](program: Console ?=> A): State[IO] ?=> A =
    program(using
      new:
        override def readLine(): Abort[ConsoleError] ?=> String = modify(_.readLine())

        override def println(text: String): Unit = update(_.appendOutput(s"$text\n"))
    )

  /**
   * Run a program with the given input.
   *
   * @param input the input to provide to the program
   * @param program the program to run
   * @return the output and program result
   */
  def withInput[A](input: String)(program: Console ?=> A): (String, A) =
    val (io, result) = State(IO(input, ""))(Console(program))
    (io.output, result)

  /**
   * Run a program using the standard console.
   *
   * @param program the program to run
   * @return the program result
   */
  def withStd[A](program: Console ?=> A): A =
    program(using
      new:
        def readLine(): Abort[ConsoleError] ?=> String =
          val line = StdIn.readLine()
          if line == null then fail(ConsoleError.EndOfInput)
          else line

        override def println(text: String): Unit = scala.Console.println(text)
    )

  /**
   * Read a line from the current console.
   *
   * @return the input line
   */
  def readLine()(using console: Console): Abort[ConsoleError] ?=> String = console.readLine()

  /**
   * Read an integer from the current console.
   *
   * @return the input integer
   */
  def readInt()(using console: Console): Abort[ConsoleError] ?=> Int =
    val line = readLine()
    line
      .toIntOption
      .getOrElse(fail(ConsoleError.InvalidInt(line)))

  /**
   * Read a floating-point number from the current console.
   *
   * @return the input number
   */
  def readDouble()(using console: Console): Abort[ConsoleError] ?=> Double =
    val line = readLine()
    line
      .toDoubleOption
      .getOrElse(fail(ConsoleError.InvalidFloat(line)))

  /**
   * Print a line to the current console.
   *
   * @param text the text to print
   */
  def println(text: String)(using console: Console): Unit = console.println(text)
