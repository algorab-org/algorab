package org.algorab

import org.algorab.ast.typed.Program
import org.algorab.compilation.Compilation
import org.algorab.compilation.Compiler
import org.algorab.parsing.AlgorabParser
import org.algorab.parsing.ExprParser
import org.algorab.parsing.TokenLexer
import org.algorab.resolution.Resolution
import org.algorab.resolution.Resolver
import org.algorab.runtime.VM
import org.algorab.typing.Typer
import org.algorab.util.FileName
import org.algorab.show.ShowContext
import org.algorab.parsing.FileInfo

/**
  * Analyze an Algorab program represented by its sources.
  * It's typically all the frontend phases.
  *
  * @param sources the program sources
  * @return the typed modules
  */
def analyzeProgram(sources: (FileName, String)*): AlgorabProgram[Seq[Program]] =
  val sourceInfos = sources.map((name, source) => name -> (FileInfo.fromSource(name, source), source))
  AlgorabProgram.registerSources(sourceInfos.toMap)
  val parsed = sourceInfos.map((_, infoSource) => AlgorabParser(infoSource._1, infoSource._2))
  val (resolvedContext, resolvedPrograms) = Resolver(parsed)
  AlgorabProgram.registerSymbols(resolvedContext.symbols)
  
  Typer(resolvedContext.symbols, resolvedContext.declarations)(resolvedPrograms)

/**
 * Run an Algorab program.
 *
 * @param sources the sources of the program, typically a [[String]] by source file
 * @return currently a sequence of typed programs, probably [[Unit]] or an exit code in the future.
 */
def runProgram(sources: (FileName, String)*): AlgorabProgram[Unit] =
  val typed = AlgorabProgram.abortIfErrors(analyzeProgram(sources*))
  val compiled = Compiler(typed)

  VM(compiled)
