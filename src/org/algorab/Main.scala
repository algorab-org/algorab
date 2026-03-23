package org.algorab

import kyo.*
import org.algorab.parser.Lexer
import scala.io.Source
import java.io.File
import scala.util.Using

object Main extends KyoApp:

  run:
    direct:
      val code = Using.resource(Source.fromFile("test/resources/golden/good/parseOp.algo"))(_.mkString)

      // val code = """val y: Int = 5
      //              |def f(x: Int): Int = x + y
      //              |
      //              |
      //              |println(f(4))""".stripMargin
      
      // Console.printLine(parse(code)).now

      // Console.printLine(compile(code).map(_.zipWithIndex.map((instr, i) => s"$i: $instr").mkString("\n"))).now
      // Console.printLine("======================").now
      runCode(code).map(Console.printLine).now