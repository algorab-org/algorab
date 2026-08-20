package org.algorab

import org.algorab.ast.resolved
import org.algorab.parsing.ExprParser
import org.algorab.parsing.TokenLexer
import org.algorab.resolution.Resolution
import org.algorab.resolution.Resolver

def runProgram(sources: String*): AlgorabProgram[Seq[resolved.Program]] =
  val parsed = sources.map(TokenLexer.apply andThen ExprParser.apply)
  val resolved = Resolution:
    parsed
      .map(ast => (ast, Resolver.declareProgram(ast)))
      .map:
        case (ast, (packageId, packageScope)) => Resolver.resolveProgram(ast, packageId, packageScope)
        
  resolved._2