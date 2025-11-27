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
      val code = """val x = 5
                   |def f(y: Int): Int =
                   |  def g(): Int =
                   |    val z = 5
                   |    x + y + z
                   |  g()""".stripMargin
                  
      Console.printLine(compile(code).map(_.zipWithIndex.map((instr, i) => s"$i: $instr").mkString("\n"))).now
      // Console.printLine("======================").now
      // runCode(code).map(Console.printLine).now