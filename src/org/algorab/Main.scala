package org.algorab

import kyo.*
import org.algorab.parser.Lexer
import scala.io.Source

object Main extends KyoApp:

  run:
    val code = """$ + $ 5""".stripMargin
    for
      _ <- Console.printLine("Run")
      result <- Parse.runResult(code)(Lexer.parseTokens)
      _ <- Console.printLine(result)
    yield
      ()