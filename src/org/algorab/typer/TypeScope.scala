package org.algorab.typer

import kyo.*
import org.algorab.ast.Identifier
import org.algorab.ast.tpd.Type

enum TypeScope:
  case Block(types: Map[Identifier, Type], variables: Map[Identifier, Variable])
  case Function(name: Identifier, types: Map[Identifier, Type], variables: Map[Identifier, Variable], captures: Set[Identifier])

  def types: Map[Identifier, Type]

  def variables: Map[Identifier, Variable]

  def getType(name: Identifier): Option[Type] =
    types.get(name)

  def withType(name: Identifier, tpe: Type): TypeScope = this match
    case Block(types, variables) => Block(types.updated(name, tpe), variables)
    case Function(functionName, types, variables, captures) => Function(functionName, types.updated(name, tpe), variables, captures)

  def getVariable(name: Identifier): Option[Variable] =
    variables.get(name)

  def withVariable(name: Identifier, variable: Variable): TypeScope = this match
    case Block(types, variables) => Block(types, variables.updated(name, variable))
    case Function(functionName, types, variables, captures) => Function(functionName, types, variables.updated(name, variable), captures)