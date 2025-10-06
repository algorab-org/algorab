package org.algorab

import org.algorab.ast.untpd.Expr
import kyo.*
import org.algorab.parser.Lexer
import org.algorab.parser.Parser

def parse(code: String): ParseResult[Expr] =
  direct:
    val lexResult = Parse.runResult(code)(Lexer.parseTokens).now
    lexResult.out match
      case Absent => ParseResult(lexResult.errors, Absent, lexResult.fatal)
      case Present(tokens) =>
        val parseResult = Parse.runResult(tokens)(Parser.parseAst).now
        parseResult.copy(errors = lexResult.errors ++ parseResult.errors)
  .eval