package org.algorab

import org.algorab.ast.typed.Program
import org.algorab.parsing.ExprParser
import org.algorab.parsing.TokenLexer
import org.algorab.resolution.Resolution
import org.algorab.resolution.Resolver
import org.algorab.typing.Typer

def runProgram(sources: String*): AlgorabProgram[Seq[Program]] =
  val parsed = sources.map(TokenLexer.apply andThen ExprParser.apply)
  val (resolvedContext, resolvedPrograms) = Resolution:
    parsed
      .map(ast => (ast, Resolver.declareProgram(ast)))
      .map:
        case (ast, (packageId, packageScope)) => Resolver.resolveProgram(ast, packageId, packageScope)

  val typed = Typer(resolvedContext.symbols, resolvedContext.declarations)(resolvedPrograms)

  typed
