package org.algorab

import kyo.*
import org.algorab.parser.Lexer
import scala.io.Source
import java.io.File
import scala.util.Using

object Main extends KyoApp:

  run:
    direct:
      // val code = Using.resource(Source.fromFile("test/resources/golden/good/core013.algo"))(_.mkString)

      val code = """def shouldNotCall(): Boolean =
                   |  println("Nope")
                   |  true
                   |
                   |-- "Nope" should not be printed
                   |
                   |if true or shouldNotCall() then println("1")
                   |if not (false and shouldNotCall()) then println("2")""".stripMargin
      
      // Console.printLine(parse(code)).now

      // Console.printLine(compile(code).map(_.zipWithIndex.map((instr, i) => s"$i: $instr").mkString("\n"))).now
      // Console.printLine("======================").now
      runCode(code).map(Console.printLine).now