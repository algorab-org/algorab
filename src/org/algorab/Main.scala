package org.algorab

import kyo.*
import org.algorab.parser.Lexer
import scala.io.Source
import java.io.File
import scala.util.Using

object Main extends KyoApp:

  run:
    direct:
      val code = Using.resource(Source.fromFile("test/resources/golden/good/evalOrder.algo"))(_.mkString)

      // val code = """def left(): Int =
      //              |  println("left")
      //              |  1
      //              |
      //              |def right(): Int =
      //              |  println("right")
      //              |  2
      //              |
      //              |def f(x: Int, y: Int): Int = x - y
      //              |
      //              |---
      //              |left
      //              |right
      //              |3
      //              |---
      //              |println(f(left(), right()))""".stripMargin
      
      // Console.printLine(parse(code)).now

      Console.printLine(compile(code).map(_.zipWithIndex.map((instr, i) => s"$i: $instr").mkString("\n"))).now
      Console.printLine("======================").now
      runCode(code).map(Console.printLine).now