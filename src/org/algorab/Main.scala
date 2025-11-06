package org.algorab

import kyo.*
import org.algorab.parser.Lexer
import scala.io.Source
import java.io.File
import scala.util.Using

object Main extends KyoApp:

  /* 

   */

  run:
    // val code = Source.fromFile("test/resources/golden/good/core019.algo").mkString
    val code = """mut val x = 5
                 |x = 6""".stripMargin
                 
    Console.printLine(compile(code))