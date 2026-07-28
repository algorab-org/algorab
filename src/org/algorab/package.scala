package org.algorab

import org.algorab.ast.resolved
import org.algorab.parsing.ExprParser
import org.algorab.parsing.TokenLexer
import org.algorab.resolution.Resolution
import org.algorab.resolution.Resolver

def runProgram(source: String): AlgorabProgram[resolved.Expr] =
  val parsed = ExprParser(TokenLexer(source))
  val resolved = Resolution(Resolver.resolveExpr(parsed))
  resolved._2
