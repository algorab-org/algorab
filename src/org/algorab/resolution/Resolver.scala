package org.algorab.resolution

import org.algorab.ast.Expr

object Resolver:

  def resolve(expr: Expr): Resolution[Expr] = expr