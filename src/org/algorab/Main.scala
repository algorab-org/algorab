package org.algorab

import kyo.*
import org.algorab.parser.Lexer
import scala.io.Source
import java.io.File
import scala.util.Using

object Main extends KyoApp:

  //val g = <ref vers fonction g>
  //g()

  //<ref vers g>()

  run:
    direct:
      // val code = Source.fromFile("test/resources/golden/good/fizzbuzz.algo").mkString
      val code = """val x = 5
                   |def f(y: Int): Int =
                   |  def g(): Int =
                   |    val z = 5
                   |    x + y + z
                   |  g()
                   |println(f(4))""".stripMargin

      // val code = """def f(x: Int): Int => Int =
      //              |  def g(y: Int): Int = x * y
      //              |  g
      //              |
      //              |println(f(6)(5))
      //              |
      //              |val h = f(6)
      //              |println(h(5))""".stripMargin

      Console.printLine(compile(code).map(_.zipWithIndex.map((instr, i) => s"$i: $instr").mkString("\n"))).now
      Console.printLine("======================").now
      runCode(code).map(Console.printLine).now