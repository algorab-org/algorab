package org.algorab

import kyo.*
import org.algorab.parser.Lexer
import scala.io.Source
import java.io.File
import scala.util.Using

object Main extends KyoApp:

  /*
  def f(x: Int): Int => Int =
    def g(y: Int): Int = x + y
    g

  val foo = f(5)
  foo(9)
  */

  run:
    direct:
      // val code = Source.fromFile("test/resources/golden/good/core019.algo").mkString
      val code = """val arr = Array(1, 2, 3)
                   |println(length(arr))
                   |mut val sum = 0
                   |
                   |for i in arr do
                   |  sum = sum + i
                   |
                   |println(sum)
                   |println(get(arr, 2))""".stripMargin
                  
      // Console.printLine(compile(code).map(_.zipWithIndex.map((instr, i) => s"$i: $instr").mkString("\n"))).now
      // Console.printLine("======================").now
      runCode(code).map(Console.printLine).now