package org.algorab

import org.algorab.ast.typed.Program
import org.algorab.parsing.ExprParser
import org.algorab.parsing.TokenLexer
import org.algorab.resolution.Resolution
import org.algorab.resolution.Resolver
import org.algorab.typing.Typer
import org.algorab.compilation.Compilation

/**
 * Run an Algorab program.
 *
 * @param sources the sources of the program, typically a [[String]] by source file
 * @return currently a sequence of typed programs, probably [[Unit]] or an exit code in the future.
 */
def runProgram(sources: String*): AlgorabProgram[Seq[Program]] =
  val parsed = sources.map(TokenLexer.apply andThen ExprParser.apply)
  val (resolvedContext, resolvedPrograms) = Resolver(parsed)
  val typed = Typer(resolvedContext.symbols, resolvedContext.declarations)(resolvedPrograms)

  typed
