package org.algorab.typing

import org.algorab.ast.*

def isSubType(subType: tpd.Type, superType: tpd.Type): Boolean =
  subType == superType

def cast(expr: tpd.Expr, expected: tpd.Type): Typing[tpd.Expr] =
  if isSubType(expr.tpe, expected) then expr
  else if isSubType(expr.tpe, tpd.Type.Int) && isSubType(tpd.Type.Float, expected) then ???
  else
    Typing.error(TypeError.mismatchExpr(List(expected), expr))
    expr.withType(expected)

def resolveType(tpe: untpd.Type): tpd.Type = tpe match
  case untpd.Type.Ref(name) => tpd.Type.Class(name)
  case untpd.Type.Inferred => throw AssertionError("Tried to resolve Type.Inferred")

  