package org.algorab

import kyo.*
import org.algorab.parser.Lexer
import scala.io.Source
import java.io.File
import scala.util.Using

object Main extends KyoApp:

  run:
    // val code = Source.fromFile("test/resources/golden/good/core019.algo").mkString
    val code = """def y(): Unit = x()
                 |def x(): Unit = Unit""".stripMargin
                 
    Console.printLine(compile(code))