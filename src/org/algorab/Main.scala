package org.algorab

import kyo.*
import org.algorab.parser.Lexer
import scala.io.Source
import java.io.File
import scala.util.Using

object Main extends KyoApp:

  run:
    direct:
      // val code = Source.fromFile("test/resources/golden/good/core019.algo").mkString
      val code = """val n = 5
                   |
                   |if n % 2 == 0 then println("pair")
                   |else println("impair")
                   |
                   |val parity = if n % 2 == 0 then "pair" else "impair"
                   |println(parity)
                   |""".stripMargin
                  
      // Console.printLine(compile(code).map(_.zipWithIndex.map((instr, i) => s"$i: $instr").mkString("\n"))).now
      // Console.printLine("======================").now
      runCode(code).map(Console.printLine).now