package org.algorab

import org.algorab.ast.tpd
import org.algorab.ast.untpd
import kyo.*
import org.algorab.parser.Lexer
import org.algorab.parser.Parser
import org.algorab.typer.Typing
import org.algorab.typer.Typer

private[algorab] def assertionError(msg: String): Nothing =
  throw AssertionError(msg)

def parse(code: String): ParseResult[untpd.Expr] =
  direct:
    val lexResult = Parse.runResult(code)(Lexer.parseTokens).now
    lexResult.out match
      case Absent => ParseResult(lexResult.errors, Absent, lexResult.fatal)
      case Present(tokens) =>
        val parseResult = Parse.runResult(tokens)(Parser.parseAst).now
        parseResult.copy(errors = lexResult.errors ++ parseResult.errors)
  .eval

def compile(code: String): Result[Chunk[CompilerFailure], tpd.Expr] =
  val parsed = parse(code)
  parsed.out match
    case Absent => Result.Failure(parsed.errors)
    case Present(expr) =>
      Typing.run(Typer.typeExpr(expr))
        .eval
        .mapFailure(parsed.errors ++ _)