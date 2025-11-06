package org.algorab.typer

import kyo.*
import org.algorab.ast.Identifier
import org.algorab.ast.tpd.Type

case class TypeScope(types: Map[Identifier, Type], variables: Map[Identifier, Variable]):

  def getType(name: Identifier): Option[Type] =
    types.get(name)

  def withType(name: Identifier, tpe: Type): TypeScope =
    this.copy(types = types.updated(name, tpe))

  def getVariable(name: Identifier): Option[Variable] =
    variables.get(name)

  def withVariable(name: Identifier, variable: Variable): TypeScope =
    this.copy(variables = variables.updated(name, variable))