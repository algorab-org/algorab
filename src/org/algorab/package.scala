package org.algorab

import org.algorab.ast.resolved
import org.algorab.parsing.ExprParser
import org.algorab.parsing.TokenLexer
import org.algorab.resolution.Resolution
import org.algorab.resolution.Resolver

def runProgram(source: String): AlgorabProgram[resolved.Program] =
  val parsed = ExprParser(TokenLexer(source))
  val resolved = Resolution(Resolver.resolveProgram(parsed))
  resolved._2
