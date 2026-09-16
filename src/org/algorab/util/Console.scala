package org.algorab.util

import purelogic.*
import scala.io.StdIn

trait Console:

  def readLine(): String

  def readInt(): Int

  def readDouble(): Double

  def println(text: String): Unit

object Console:

  case class IO(input: String, output: String):

    def readLine(): (String, IO) =
      val endPos = input.indexWhere(c => c == '\n' || c == '\r')
      if endPos == -1 then (input, this.copy(input = ""))
      else if input(endPos) == '\r' && input(endPos + 1) == '\n' then
        (input.take(endPos), this.copy(input = input.drop(endPos + 2)))
      else
        (input.take(endPos), this.copy(input = input.drop(endPos + 1)))

    def appendOutput(output: String): IO = this.copy(output = this.output + output)

  def apply[A](program: Console ?=> A): State[IO] ?=> A =
    program(using new:
      override def readLine(): String = modify(_.readLine())

      override def readInt(): Int = readLine().toInt

      override def readDouble(): Double = readLine().toDouble

      override def println(text: String): Unit = update(_.appendOutput(s"$text\n"))
    )

  def withInput[A](input: String)(program: Console ?=> A): (String, A) =
    val (io, result) = State(IO(input, ""))(Console(program))
    (io.output, result)

  def withStd[A](program: Console ?=> A): A =
    program(using new:
      def readLine(): String = StdIn.readLine()

      override def readInt(): Int = StdIn.readInt()
      
      override def readDouble(): Double = StdIn.readDouble()

      override def println(text: String): Unit = scala.Console.println(text)  
    )

  def readLine()(using console: Console): String = console.readLine()

  def readInt()(using console: Console): Int = console.readInt()
  
  def readDouble()(using console: Console): Double = console.readDouble()

  def println(text: String)(using console: Console): Unit = console.println(text)