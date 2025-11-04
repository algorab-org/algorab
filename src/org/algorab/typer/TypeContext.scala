package org.algorab.typer

import kyo.*
import org.algorab.ast.Identifier
import org.algorab.ast.Type

case class TypeContext(scopes: Chunk[TypeScope]):

  def getType(name: Identifier): Option[Type] =
    scopes.collectFirst[Type](((scope: TypeScope) => scope.getType(name)).unlift)

  def getTypeOrFail(name: Identifier): Type < Typing =
    getType(name) match
      case Some(value) => value
      case None => Typing.failAndAbort(TypeFailure.UnknownType(name))

  def getVariable(name: Identifier): Option[Type] =
    scopes.collectFirst(((scope: TypeScope) => scope.getVariable(name)).unlift)
  
  def getVariableOrFail(name: Identifier): Type < Typing =
    getVariable(name) match
      case Some(value) => value
      case None => Typing.failAndAbort(TypeFailure.UnknownVariable(name))

  def declareVariable(name: Identifier, tpe: Type): TypeContext < Typing =
    scopes.head.getVariable(name) match
      case Some(_) => Typing.failAndAbort(TypeFailure.VariableAlreadyDefined(name))
      case None => updateVariable(name, tpe)

  def updateVariable(name: Identifier, tpe: Type): TypeContext < Typing =
    this.copy(scopes = scopes.head.withVariable(name, tpe) +: scopes.tail)

  def merge(inner: TypeContext): TypeContext =
    this.copy(scopes = inner.scopes.takeRight(scopes.length))

object TypeContext:

  val default: TypeContext = TypeContext(Chunk(
    TypeScope(
      types = Map.empty,
      variables = Map(
        Identifier("Unit") -> Type.Unit,
        Identifier("println") -> Type.Fun(Chunk(Type.Any), Type.Unit),
        Identifier("readInt") -> Type.Fun(Chunk.empty, Type.Int),
        Identifier("readFloat") -> Type.Fun(Chunk.empty, Type.Float)
      )
    )
  ))

  def getType(name: Identifier): Option[Type] < Typing = Var.use(_.getType(name))

  def getTypeOrFail(name: Identifier): Type < Typing = Var.use(_.getTypeOrFail(name))

  def getVariable(name: Identifier): Option[Type] < Typing = Var.use(_.getVariable(name))

  def getVariableOrFail(name: Identifier): Type < Typing = Var.use(_.getVariableOrFail(name))

  def declareVariable(name: Identifier, tpe: Type): Unit < Typing =
    Var
      .use[TypeContext](_.declareVariable(name, tpe))
      .map(Var.set)
      .unit

  def updateVariable(name: Identifier, tpe: Type): Unit < Typing =
    Var
      .use[TypeContext](_.updateVariable(name, tpe))
      .map(Var.set)
      .unit

  def inNewScope[A](body: A < Typing): A < Typing =
    Var.isolate.merge[TypeContext](_.merge(_)).run(
      Var.update[TypeContext](ctx => ctx.copy(scopes = TypeScope(Map.empty, Map.empty) +: ctx.scopes))
        .andThen(body)
    )

  def isSubtype(tpe: Type, expected: Type): Boolean < Typing = (tpe, expected) match
    case (_, Type.Any) => true
    case (Type.Ref(Identifier("Int")), Type.Ref(Identifier("Float"))) => true
    case (_, Type.Inferred) => true
    case _ => tpe == expected