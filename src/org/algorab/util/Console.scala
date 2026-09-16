package org.algorab.util

import purelogic.*
import scala.io.StdIn
import org.algorab.util.ConsoleError
import java.io.IOException

trait Console:

  def readLine(): Abort[ConsoleError] ?=> String

  def println(text: String): Unit

object Console:

  case class IO(input: String, output: String):

    def readLine(): Abort[ConsoleError] ?=> (String, IO) =
      if input.isEmpty then fail(ConsoleError.EndOfInput)
      else
        val endPos = input.indexWhere(c => c == '\n' || c == '\r')
        if endPos == -1 then (input, this.copy(input = ""))
        else if input(endPos) == '\r' && input(endPos + 1) == '\n' then
          (input.take(endPos), this.copy(input = input.drop(endPos + 2)))
        else
          (input.take(endPos), this.copy(input = input.drop(endPos + 1)))

    def appendOutput(output: String): IO = this.copy(output = this.output + output)

  def apply[A](program: Console ?=> A): State[IO] ?=> A =
    program(using new:
      override def readLine(): Abort[ConsoleError] ?=> String = modify(_.readLine())

      override def println(text: String): Unit = update(_.appendOutput(s"$text\n"))
    )

  def withInput[A](input: String)(program: Console ?=> A): (String, A) =
    val (io, result) = State(IO(input, ""))(Console(program))
    (io.output, result)

  def withStd[A](program: Console ?=> A): A =
    program(using new:
      def readLine(): Abort[ConsoleError] ?=> String =
        val line = StdIn.readLine()
        if line == null then fail(ConsoleError.EndOfInput)
        else line

      override def println(text: String): Unit = scala.Console.println(text)  
    )

  def readLine()(using console: Console): Abort[ConsoleError] ?=> String = console.readLine()

  def readInt()(using console: Console): Abort[ConsoleError] ?=> Int =
    val line = readLine()
    line
      .toIntOption
      .getOrElse(fail(ConsoleError.InvalidInt(line)))
  
  def readDouble()(using console: Console): Abort[ConsoleError] ?=> Double =
    val line = readLine()
      line
        .toDoubleOption
        .getOrElse(fail(ConsoleError.InvalidFloat(line)))

  def println(text: String)(using console: Console): Unit = console.println(text)