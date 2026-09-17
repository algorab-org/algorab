package org.algorab

import org.algorab.ast.typed.Program
import org.algorab.compilation.Compilation
import org.algorab.compilation.Compiler
import org.algorab.parsing.ExprParser
import org.algorab.parsing.TokenLexer
import org.algorab.resolution.Resolution
import org.algorab.resolution.Resolver
import org.algorab.runtime.VM
import org.algorab.typing.Typer

def analyzeProgram(sources: String*): AlgorabProgram[Seq[Program]] =
  val parsed = sources.map(TokenLexer.apply andThen ExprParser.apply)
  val (resolvedContext, resolvedPrograms) = Resolver(parsed)
  Typer(resolvedContext.symbols, resolvedContext.declarations)(resolvedPrograms)

/**
 * Run an Algorab program.
 *
 * @param sources the sources of the program, typically a [[String]] by source file
 * @return currently a sequence of typed programs, probably [[Unit]] or an exit code in the future.
 */
def runProgram(sources: String*): AlgorabProgram[Unit] =
  val typed = AlgorabProgram.abortIfErrors(analyzeProgram(sources*))
  val compiled = Compiler(typed)

  VM(compiled)
