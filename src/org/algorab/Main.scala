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
      val code = Source.fromFile("test/resources/golden/good/core031.algo").mkString

      // val code = """--- variable shadowing and scope ---
      //              |mut val i: Int = 78
      //              |println(i)
      //              |while i > 76 do
      //              |  i = i - 1
      //              |  println(i)
      //              |  println(i + 7)
      //              |println(i)
      //              |if i > 4 then
      //              |  mut val i: Int = 4
      //              |  println(i)
      //              |else
      //              |  println("foo")
      //              |println(i)""".stripMargin
      
      // Console.printLine(parse(code)).now

      // Console.printLine(compile(code).map(_.zipWithIndex.map((instr, i) => s"$i: $instr").mkString("\n"))).now
      // Console.printLine("======================").now
      runCode(code).map(Console.printLine).now