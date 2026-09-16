package org.algorab

import org.algorab.ast.compiled.Program
import org.algorab.compilation.Compilation
import org.algorab.compilation.Compiler
import org.algorab.parsing.ExprParser
import org.algorab.parsing.TokenLexer
import org.algorab.resolution.Resolution
import org.algorab.resolution.Resolver
import org.algorab.typing.Typer
import org.algorab.runtime.VM

/**
 * Run an Algorab program.
 *
 * @param sources the sources of the program, typically a [[String]] by source file
 * @return currently a sequence of typed programs, probably [[Unit]] or an exit code in the future.
 */
def runProgram(sources: String*): AlgorabProgram[Unit] =
  val parsed = sources.map(TokenLexer.apply andThen ExprParser.apply)
  val (resolvedContext, resolvedPrograms) = Resolver(parsed)
  val typed = Typer(resolvedContext.symbols, resolvedContext.declarations)(resolvedPrograms)
  val compiled = Compiler(typed)

  VM(compiled)