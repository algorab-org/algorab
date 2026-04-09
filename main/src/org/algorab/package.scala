/** The Algorab compiler and interpreter.
  *
  * This package contains the top-level pipeline entry points that chain the individual compilation
  * phases together:
  *
  *   1. [[parse]] – Lex and parse source text into an untyped AST (`untpd.Expr`).
  *   1. [[compile]] – Type-check and compile the untyped AST to a flat sequence of [[compiler.Instruction]]s.
  *   1. [[runCode]] – Execute a source string end-to-end through all phases and the virtual machine.
  *
  * Typical call order:
  * {{{
  *   val result: Result[Chunk[CompilerFailure], Unit] < Runtime.Execution =
  *     runCode(sourceCode)
  * }}}
  *
  * @see [[parser.Lexer]], [[parser.Parser]], [[typer.Typer]], [[compiler.Compiler]], [[runtime.VM]]
  */
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

/** Throws an [[AssertionError]] with the given message.
  *
  * Used internally to signal invariant violations that should never occur in a
  * well-formed program. Prefer specific error types in public APIs.
  *
  * @param msg human-readable description of the violated invariant
  * @return Nothing – this method never returns normally
  */
private[algorab] def assertionError(msg: String): Nothing =
  throw AssertionError(msg)

/** Lex and parse Algorab source code into an untyped expression tree.
  *
  * The function runs two sequential phases:
  *   - '''Lexing''': tokenises the raw source string via [[parser.Lexer.parseTokens]],
  *     handling indentation layout and comments.
  *   - '''Parsing''': transforms the token stream into a `untpd.Expr` tree via
  *     [[parser.Parser.parseAst]].
  *
  * Lexer errors are always accumulated; parser errors are only collected when
  * the lexer produced at least a partial token stream.
  *
  * @param code the raw Algorab source code
  * @return a [[ParseResult]] containing any parse errors and, if successful,
  *         the resulting [[ast.untpd.Expr]] tree
  */
def parse(code: String): ParseResult[untpd.Expr] =
  direct:
    val lexResult = Parse.runResult(code)(Lexer.parseTokens).now
    lexResult.out match
      case Absent => ParseResult(lexResult.errors, Absent, lexResult.fatal)
      case Present(tokens) =>
        val parseResult = Parse.runResult(tokens)(Parser.parseAst).now
        parseResult.copy(errors = lexResult.errors ++ parseResult.errors)
  .eval

/** Parse, type-check, and compile Algorab source code to bytecode instructions.
  *
  * Runs the full front-end pipeline:
  *   1. [[parse]] – produce an untyped AST (returns `Result.Failure` on parse errors).
  *   1. [[typer.Typer.typeProgram]] – type-check and annotate the AST.
  *   1. [[compiler.Compiler.compileProgram]] – lower the typed AST to a flat
  *      [[Chunk]] of [[compiler.Instruction]]s.
  *
  * @param code the raw Algorab source code
  * @return `Result.Success` containing the instruction sequence, or
  *         `Result.Failure` containing all accumulated [[CompilerFailure]]s
  */
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

/** Parse, compile, and execute Algorab source code in the virtual machine.
  *
  * Convenience wrapper that calls [[compile]] and, on success, passes the
  * resulting instruction sequence to [[runtime.VM.interpretAll]].
  *
  * @param code the raw Algorab source code
  * @return a Kyo effect that, when run inside a [[runtime.Runtime.Execution]]
  *         context, yields `Result.Success(())` on successful execution or
  *         `Result.Failure` with all [[CompilerFailure]]s on compilation errors
  */
def runCode(code: String): Result[Chunk[CompilerFailure], Unit] < Runtime.Execution =
  compile(code) match
    case Result.Failure(failures)     => Result.Failure(failures)
    case Result.Success(instructions) => Runtime.run(VM.interpretAll(instructions)).map(Result.Success.apply)
