package org.algorab.typer

import kyo.*
import org.algorab.ast.Identifier
import org.algorab.ast.tpd.Type

case class TypeContext(scopes: Chunk[TypeScope]):

  def getType(name: Identifier): Option[Type] =
    scopes.collectFirst[Type](((scope: TypeScope) => scope.getType(name)).unlift)

  def getTypeOrFail(name: Identifier): Type < Typing =
    getType(name) match
      case Some(value) => value
      case None => Typing.failAndAbort(TypeFailure.UnknownType(name))

  def declareType(name: Identifier, tpe: Type): TypeContext < Typing =
    scopes.head.getType(name) match
      case Some(_) => Typing.failAndAbort(TypeFailure.TypeAlreadyDefined(name))
      case None => updateType(name, tpe)

  def updateType(name: Identifier, tpe: Type): TypeContext < Typing =
    this.copy(scopes = scopes.head.withType(name, tpe) +: scopes.tail)

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

  def newUniqueName(baseName: Identifier): Identifier =
    val greatestId = scopes
      .flatMap(_.types.values)
      .foldLeft(-1)((curId, tpe) =>
        tpe match
          case Type.Generic(name) if name == baseName && curId == -1 => 0
          case Type.Generic(Identifier(s"$baseName$$$id")) =>
            id.toIntOption.fold(curId)(math.max(_, curId))
          case _ => curId
      )

    if greatestId == -1 then baseName
    else Identifier.assume(s"$baseName$$${greatestId+1}")

object TypeContext:

  val default: TypeContext = TypeContext(Chunk(
    TypeScope(
      types = Map(
        Identifier("Any") -> Type.Any,
        Identifier("Unit") -> Type.Unit,
        Identifier("Boolean") -> Type.Boolean,
        Identifier("Int") -> Type.Int,
        Identifier("Float") -> Type.Float,
        Identifier("Char") -> Type.Char,
        Identifier("String") -> Type.String,
        Identifier("Array") -> Type.Array,
      ),
      variables = Map(
        Identifier("Unit") -> Type.Unit,
        Identifier("println") -> Type.Fun(Chunk(Type.Any), Type.Unit),
        Identifier("readInt") -> Type.Fun(Chunk.empty, Type.Int),
        Identifier("readFloat") -> Type.Fun(Chunk.empty, Type.Float)
      )
    )
  ))

  def modify(f: TypeContext => TypeContext < Typing): Unit < Typing =
    Var.use[TypeContext](f)
      .map(Var.set)
      .unit

  def getType(name: Identifier): Option[Type] < Typing = Var.use(_.getType(name))

  def getTypeOrFail(name: Identifier): Type < Typing = Var.use(_.getTypeOrFail(name))

  def declareType(name: Identifier, tpe: Type): Unit < Typing = modify(_.declareType(name, tpe))

  def updateType(name: Identifier, tpe: Type): Unit < Typing = modify(_.updateType(name, tpe))

  def getVariable(name: Identifier): Option[Type] < Typing = Var.use(_.getVariable(name))

  def getVariableOrFail(name: Identifier): Type < Typing = Var.use(_.getVariableOrFail(name))

  def declareVariable(name: Identifier, tpe: Type): Unit < Typing = modify(_.declareVariable(name, tpe))

  def updateVariable(name: Identifier, tpe: Type): Unit < Typing = modify(_.updateVariable(name, tpe))
  
  def newUniqueName(name: Identifier): Identifier < Typing = Var.use(_.newUniqueName(name))

  def inNewScope[A](body: A < Typing): A < Typing =
    Var.isolate.merge[TypeContext](_.merge(_)).run(
      Var.update[TypeContext](ctx => ctx.copy(scopes = TypeScope(Map.empty, Map.empty) +: ctx.scopes))
        .andThen(body)
    )

  def isSubtype(tpe: Type, expected: Type): Boolean < Typing = (tpe, expected) match
    case (_, Type.Any) => true
    case (Type.Int, Type.Float) => true
    case (_, Type.Inferred) => true
    case _ => tpe == expected

  def union(typeA: Type, typeB: Type): Type < Typing = (typeA, typeB) match
    case (Type.Nothing, _) => typeB
    case (_, Type.Nothing) => typeA
    case (Type.Int, Type.Float) => Type.Float
    case _ =>
      if typeA == typeB then typeA
      else Type.Any  