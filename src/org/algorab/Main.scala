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
      val code = Source.fromFile("test/resources/golden/good/core029.algo").mkString

      // val code = """def fac(x: Int): Int =
      //              |  if x == 0 then 1
      //              |  else x * fac(x - 1)
      //              |
      //              |println(fac(5))""".stripMargin

      // val code = """def f(): Int => Int =
      //              |  mut val x = 0
      //              |  def g(y: Int): Int =
      //              |    x = x + y
      //              |    x
      //              |  g
      //              |
      //              |val g = f()
      //              |println(g(1))
      //              |println(g(2))
      //              |println(g(-3))""".stripMargin

      /* 
      Declare("x")
      Push(5)
      Assign("x")
      Load("x")
      Load("println")
      Apply(1)
      */

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
      Console.withIn(Chunk("101325.0"))(runCode(code).map(Console.printLine)).now