package org.algorab.typer

import kyo.*
import org.algorab.ast.Identifier
import org.algorab.ast.tpd.Type

enum TypeScope:
  case Block(types: Map[Identifier, Type], variables: Map[Identifier, VariableId])
  case Function(id: VariableId, types: Map[Identifier, Type], variables: Map[Identifier, VariableId], captures: Set[Identifier])
  case Class(id: VariableId, types: Map[Identifier, Type], variables: Map[Identifier, VariableId])

  def types: Map[Identifier, Type]

  def variables: Map[Identifier, VariableId]

  def getType(name: Identifier): Option[Type] =
    types.get(name)

  def withType(name: Identifier, tpe: Type): TypeScope = this match
    case Block(types, variables) => Block(types.updated(name, tpe), variables)
    case Function(id, types, variables, captures) =>
      Function(id, types.updated(name, tpe), variables, captures)
    case Class(id, types, variables) =>
      Class(id, types.updated(name, tpe), variables)

  def getVariable(name: Identifier): Option[VariableId] =
    variables.get(name)

  def withVariable(name: Identifier, variable: VariableId): TypeScope = this match
    case Block(types, variables) => Block(types, variables.updated(name, variable))
    case Function(id, types, variables, captures) =>
      Function(id, types, variables.updated(name, variable), captures)
    case Class(id, types, variables) =>
      Class(id, types, variables.updated(name, variable))

  def isClassScope: Boolean = this.isInstanceOf[TypeScope.Class]
