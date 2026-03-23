package org.algorab

import kyo.*
import org.algorab.ast.tpd
import org.algorab.ast.untpd
import org.algorab.compiler.Compilation
import org.algorab.compiler.Compiler
import org.algorab.compiler.InstrPosition
import org.algorab.compiler.Instruction
import org.algorab.parser.Lexer
import org.algorab.parser.Parser
import org.algorab.runtime.Runtime
import org.algorab.runtime.VM
import org.algorab.typer.Typer
import org.algorab.typer.Typing

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

def compile(code: String): Result[Chunk[CompilerFailure], Chunk[Instruction]] =
  val parsed = parse(code)
  parsed.out match
    case Absent => Result.Failure(parsed.errors)
    case Present(expr) =>
      Typer.typeProgram(expr)
        .map((ctx, expr) => Env.run(ctx)(Compiler.compileProgram(expr)))
        .handle(
          Compilation.run(InstrPosition(0)),
          Typing.run
        )
        .eval
        .mapFailure(parsed.errors ++ _)

def runCode(code: String): Result[Chunk[CompilerFailure], Unit] < Runtime.Execution =
  compile(code) match
    case Result.Failure(failures) => Result.Failure(failures)
    case Result.Success(instructions) =>
      Runtime.run(VM.interpretAll(instructions)).map(Result.Success.apply)
