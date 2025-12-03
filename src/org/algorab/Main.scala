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
      // val code = Source.fromFile("test/resources/golden/good/core029.algo").mkString

      // val code = """def f(n: Int): Int =
      //              |  val cond = n == 0
      //              |  if n == 0 then 1 else n * g(n - 1)
      //              |def g(n: Int): Int = if n == 0 then 1 else n * f(n - 1)
      //              |
      //              |println(f(5))""".stripMargin

      val code = """val x = 5
                   |val y = 3
                   |println(x + y)""".stripMargin

      Console.printLine(compile(code).map(_.zipWithIndex.map((instr, i) => s"$i: $instr").mkString("\n"))).now
      Console.printLine("======================").now
      runCode(code).map(Console.printLine).now