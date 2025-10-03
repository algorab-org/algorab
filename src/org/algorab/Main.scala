package org.algorab

import kyo.*
import org.algorab.parser.Lexer
import scala.io.Source
import java.io.File
import scala.util.Using

object Main extends KyoApp:

  run:
    val code = Using.resource(Source.fromFile(File("test/resources/golden/good/core024.algo")))(_.mkString)
    // val code = """if x then
    //              |  y = z
    //              |else
    //              |  y = z2""".stripMargin
                 
    Console.printLine(parse(code))