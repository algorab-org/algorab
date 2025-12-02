package org.algorab.typer

import kyo.*
import org.algorab.ast.Identifier
import org.algorab.ast.tpd.Type

enum TypeScope:
  case Block(types: Map[Identifier, Type], variables: Map[Identifier, VariableId])
  case Function(types: Map[Identifier, Type], variables: Map[Identifier, VariableId], captures: Set[Identifier])

  def types: Map[Identifier, Type]

  def variables: Map[Identifier, VariableId]

  def getType(name: Identifier): Option[Type] =
    types.get(name)

  def withType(name: Identifier, tpe: Type): TypeScope = this match
    case Block(types, variables) => Block(types.updated(name, tpe), variables)
    case Function(types, variables, captures) =>
      Function(types.updated(name, tpe), variables, captures)

  def getVariable(name: Identifier): Option[VariableId] =
    variables.get(name)

  def withVariable(name: Identifier, variable: VariableId): TypeScope = this match
    case Block(types, variables) => Block(types, variables.updated(name, variable))
    case Function(types, variables, captures) =>
      Function(types, variables.updated(name, variable), captures)