package org.algorab.typer

import kyo.*
import org.algorab.ast.Identifier
import org.algorab.ast.tpd.Type

case class TypeContext(scopes: Chunk[TypeScope], functions: Map[Identifier, FunctionDef]):

  def getType(name: Identifier): Option[Type] =
    scopes.collectFirst[Type](((scope: TypeScope) => scope.getType(name)).unlift)

  def getTypeOrFail(name: Identifier): Type < Typing =
    getType(name) match
      case Some(value) => value
      case None        => Typing.failAndAbort(TypeFailure.UnknownType(name))

  def declareType(name: Identifier, tpe: Type): TypeContext < Typing =
    scopes.head.getType(name) match
      case Some(_) => Typing.failAndAbort(TypeFailure.TypeAlreadyDefined(name))
      case None    => updateType(name, tpe)

  def updateType(name: Identifier, tpe: Type): TypeContext < Typing =
    this.copy(scopes = scopes.head.withType(name, tpe) +: scopes.tail)

  def getVariable(name: Identifier): Option[Variable] =
    scopes.collectFirst[Variable](((scope: TypeScope) => scope.getVariable(name)).unlift)

  def getVariableOrFail(name: Identifier): Variable < Typing =
    getVariable(name) match
      case Some(value) => value
      case None        => Typing.failAndAbort(TypeFailure.UnknownVariable(name))

  def declareVariable(name: Identifier, variable: Variable): TypeContext < Typing =
    scopes.head.getVariable(name) match
      case Some(_) => Typing.failAndAbort(TypeFailure.VariableAlreadyDefined(name))
      case None    => updateVariable(name, variable)

  def updateVariable(name: Identifier, variable: Variable): TypeContext =
    this.copy(scopes = scopes.head.withVariable(name, variable) +: scopes.tail)

  def getFunction(name: Identifier): Option[FunctionDef] = functions.get(name)

  def declareFunction(name: Identifier, function: FunctionDef): TypeContext =
    this.copy(functions = functions.updated(name, function))

  def merge(inner: TypeContext): TypeContext =
    this.copy(scopes = inner.scopes.takeRight(scopes.length), functions = functions ++ inner.functions)

  def newUniqueTypeName(baseName: Identifier): Identifier =
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
    else Identifier.assume(s"$baseName$$${greatestId + 1}")

  def newUniqueVarName(baseName: Identifier): Identifier =
    val greatestId = scopes
      .flatMap(_.variables.keys)
      .foldLeft(-1)((curId, name) =>
        name match
          case n if n == baseName && curId == -1 => 0
          case Identifier(s"$baseName$$$id") =>
            id.toIntOption.fold(curId)(math.max(_, curId))
          case _ => curId
      )

    if greatestId == -1 then baseName
    else Identifier.assume(s"$baseName$$${greatestId + 1}")

  def newUniqueFunctionName(baseName: Identifier): Identifier =
    val greatestId = functions.keys.foldLeft(-1)((curId, name) =>
      name match
        case n if n == baseName && curId == -1 => 0
        case Identifier(s"$baseName$$$id") =>
          id.toIntOption.fold(curId)(math.max(_, curId))
        case _ => curId
    )

    if greatestId == -1 then baseName
    else Identifier.assume(s"$baseName$$${greatestId + 1}")

  def withCurrentFunction(currentFunction: Identifier): TypeContext =
    this.copy(currentFunction = Present((scopes.size - 1, currentFunction)))

object TypeContext:

  val default: TypeContext = TypeContext(
    scopes = Chunk(
      TypeScope(
        types = Map(
          Identifier("Any") -> Type.Any,
          Identifier("Unit") -> Type.Unit,
          Identifier("Boolean") -> Type.Boolean,
          Identifier("Int") -> Type.Int,
          Identifier("Float") -> Type.Float,
          Identifier("Char") -> Type.Char,
          Identifier("String") -> Type.String,
          Identifier("Array") -> Type.Array
        ),
        variables = Map(
          Identifier("Unit") -> Variable(Type.Unit, false),
          Identifier("println") -> Variable(Type.Fun(Chunk(Type.Any), Type.Unit), false),
          Identifier("readInt") -> Variable(Type.Fun(Chunk.empty, Type.Int), false),
          Identifier("readFloat") -> Variable(Type.Fun(Chunk.empty, Type.Float), false),
          // TODO Change length and Array once OOP and multifile are implemented
          Identifier("length") -> Variable(
            tpe = Type.TypeFun(
              typeParams = Chunk(Identifier("A")),
              output = Type.Fun(Chunk(Type.arrayOf(Type.Generic(Identifier("A")))), Type.Int)
            ),
            mutable = false
          ),
          Identifier("get") -> Variable(
            tpe = Type.TypeFun(
              typeParams = Chunk(Identifier("A")),
              output = Type.Fun(Chunk(Type.arrayOf(Type.Generic(Identifier("A"))), Type.Int), Type.Generic(Identifier("A")))
            ),
            mutable = false
          ),
          Identifier("Array") -> Variable(
            tpe = Type.TypeFun(
              typeParams = Chunk(Identifier("A")),
              output = Type.Fun(
                // TODO Use varargs once implemented
                Chunk(
                  Type.Generic(Identifier("A")),
                  Type.Generic(Identifier("A")),
                  Type.Generic(Identifier("A"))
                ),
                Type.arrayOf(Type.Generic(Identifier("A")))
              )
            ),
            mutable = false
          )
        )
      )
    ),
    functions = Map.empty,
    currentFunction = Maybe.empty
  )

  def modify(f: TypeContext => TypeContext < Typing): Unit < Typing =
    Var.use[TypeContext](f)
      .map(Var.set)
      .unit

  def modifyReturn[A](f: TypeContext => (TypeContext, A) < Typing): A < Typing =
    Var.use[TypeContext](f)
      .map(Var.set(_).andThen(_))

  def getType(name: Identifier): Option[Type] < Typing = Var.use(_.getType(name))

  def getTypeOrFail(name: Identifier): Type < Typing = Var.use(_.getTypeOrFail(name))

  def declareType(name: Identifier, tpe: Type): Unit < Typing = modify(_.declareType(name, tpe))

  def updateType(name: Identifier, tpe: Type): Unit < Typing = modify(_.updateType(name, tpe))

  def getVariable(name: Identifier): Option[Variable] < Typing = modifyReturn(_.getVariable(name))

  def getVariableOrFail(name: Identifier): Variable < Typing = modifyReturn(_.getVariableOrFail(name))

  def declareVariable(name: Identifier, variable: Variable): Unit < Typing = modify(_.declareVariable(name, variable))

  def updateVariable(name: Identifier, variable: Variable): Unit < Typing = modify(_.updateVariable(name, variable))

  def getFunction(name: Identifier): Option[FunctionDef] < Typing = Var.use(_.getFunction(name))

  def declareFunction(name: Identifier, function: FunctionDef): Unit < Typing = modify(_.declareFunction(name, function))

  def newUniqueTypeName(name: Identifier): Identifier < Typing = Var.use(_.newUniqueTypeName(name))

  def newUniqueVarName(name: Identifier): Identifier < Typing = Var.use(_.newUniqueVarName(name))

  def newUniqueFunctionName(name: Identifier): Identifier < Typing = Var.use(_.newUniqueFunctionName(name))

  def setCurrentFunction(name: Identifier): Unit < Typing = modify(_.withCurrentFunction(name))

  def inNewScope[A](body: A < Typing): A < Typing =
    Var.isolate.merge[TypeContext](_.merge(_)).run(
      Var.update[TypeContext](ctx => ctx.copy(scopes = TypeScope(Map.empty, Map.empty) +: ctx.scopes))
        .andThen(body)
    )

  def isSubtype(tpe: Type, expected: Type): Boolean < Typing = (tpe, expected) match
    case (_, Type.Any)          => true
    case (Type.Int, Type.Float) => true
    case (_, Type.Inferred)     => true
    case _                      => tpe == expected

  def union(typeA: Type, typeB: Type): Type < Typing = (typeA, typeB) match
    case (Type.Nothing, _)      => typeB
    case (_, Type.Nothing)      => typeA
    case (Type.Int, Type.Float) => Type.Float
    case _ =>
      if typeA == typeB then typeA
      else Type.Any
