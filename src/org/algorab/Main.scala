package org.algorab

import kyo.*
import org.algorab.parser.Lexer
import scala.io.Source
import java.io.File
import scala.util.Using

object Main extends KyoApp:

  run:
    // val code = Source.fromFile("test/resources/golden/good/core019.algo").mkString
    val code = """val x = 5
                 |val y = x + 5 * 3
                 |
                 |mut val z =
                 |  if y < x then true
                 |  else false
                 |
                 |while z do
                 |  z = false
                 |""".stripMargin
                 
    Console.printLine(compile(code).map(_.zipWithIndex.map((instr, i) => s"$i: $instr").mkString("\n")))