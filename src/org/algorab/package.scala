package org.algorab

import org.algorab.ast.Expr
import org.algorab.parsing.TokenLexer
import org.algorab.parsing.ExprParser
import org.algorab.resolution.Resolution
import org.algorab.resolution.Resolver

def runProgram(source: String): AlgorabProgram[Expr] =
  val parsed = ExprParser(TokenLexer(source))
  val resolved = Resolution(Resolver.resolve(parsed))
  resolved._2