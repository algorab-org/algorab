package org.algorab

import kyo.*
import org.algorab.parser.Lexer
import scala.io.Source
import java.io.File
import scala.util.Using

object Main extends KyoApp:

  run:
    // val code = Source.fromFile("test/resources/golden/good/core019.algo").mkString
    val code = """def id[A](x: A): A = x
                 |val f = id
                 |val g = id[String]
                 |f("hey")
                 |f(5)
                 |-- f[String](5)
                 |g("hey")
                 |-- g(5)""".stripMargin
                 
    Console.printLine(compile(code))