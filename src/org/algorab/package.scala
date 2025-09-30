package org.algorab

import org.algorab.ast.untpd.Expr
import kyo.*
import org.algorab.parser.Lexer
import org.algorab.parser.Parser

def parse(code: String): Expr < Abort[ParseError] = direct:
  val tokens = Parse.runOrAbort(code)(Lexer.parseTokens).now
  Parse.runOrAbort(tokens)(Parser.parseAst).now