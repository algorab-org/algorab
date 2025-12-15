package org.algorab.typer

import kyo.*
import org.algorab.ast.Identifier
import org.algorab.ast.tpd.Type
import scala.annotation.tailrec
import org.algorab.ast.tpd.Expr

case class TypeContext(
  scopes: Chunk[TypeScope],
  functions: Map[Identifier, FunctionDef],
  variables: Chunk[Variable]
):

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

  def getVariable(name: Identifier): (TypeContext, Option[(VariableId, Variable)]) =
    
    @tailrec
    def rec(
      scopes: Chunk[TypeScope],
      updatedScopes: Chunk[TypeScope],
      captured: Boolean
    ): (Chunk[TypeScope], Option[(VariableId, Variable)]) = scopes match
      case head +: tail =>
        head match
          case TypeScope.Block(_, variables) => variables.get(name) match
            case None => rec(tail, updatedScopes :+ head, captured)
            case Some(id) =>
              var variable = this.variables(id.value)
              if captured && variable.mutable then variable = variable.copy(boxxed = true)
              (updatedScopes ++ scopes, Some((id, variable)))
          case TypeScope.Function(fid, types, variables, captures) => variables.get(name) match
            case None => rec(tail, updatedScopes :+ TypeScope.Function(fid, types, variables, captures + name), true)
            case Some(id) =>
              var variable = this.variables(id.value)
              if captured && variable.mutable then variable = variable.copy(boxxed = true)
              (updatedScopes ++ scopes, Some((id, variable)))

      case _ => (updatedScopes ++ scopes, None)

    val (updatedScopes, variable) = rec(scopes, Chunk.empty, false)
    variable match
      case Some((id, v)) if v.boxxed =>
        val st = this.copy(variables = variables.updated(id.value, v), scopes = updatedScopes)
        (st, variable)
      case _ =>
        (this.copy(scopes = updatedScopes), variable)

  def getVariableOrFail(name: Identifier): (TypeContext, (VariableId, Variable)) < Typing =
    getVariable(name) match
      case (newCtx, Some(value)) => (newCtx, value)
      case (_, None)        => Typing.failAndAbort(TypeFailure.UnknownVariable(name))

  def getVariableId(name: Identifier): VariableId =
    scopes.collectFirst(((scope: TypeScope) => scope.getVariable(name)).unlift).get

  def declareVariable(name: Identifier, variable: Variable): (TypeContext, VariableId) < Typing =
    scopes.head.getVariable(name) match
      case Some(_) => Typing.failAndAbort(TypeFailure.VariableAlreadyDefined(name))
      case None    =>
        val id = VariableId.assume(variables.size)
        (
          this.copy(
            scopes = scopes.head.withVariable(name, id) +: scopes.tail,
            variables = variables :+ variable
          ),
          id
        )

  def declareVariableForce(name: Identifier, tpe: Type): TypeContext =
    this.copy(
      scopes = scopes.head.withVariable(name, VariableId.assume(variables.size)) +: scopes.tail,
      variables = variables :+ Variable(name, tpe, false, false)
    )

  def updateVariable(name: Identifier, variable: Variable): TypeContext =
    this.copy(
      variables = variables.updated(scopes.head.variables(name).value, variable)
    )

  def getFunction(name: Identifier): Option[FunctionDef] = functions.get(name)

  def mergeBlock(inner: TypeContext): TypeContext =
    this.copy(scopes = inner.scopes.drop(1), functions = inner.functions, variables = inner.variables)

  def popFunction(name: Identifier, displayName: Identifier, params: Chunk[Identifier], body: Expr): TypeContext = scopes match
    case TypeScope.Function(id, types, variables, localCaptures) +: remaining =>
      this.copy(
        scopes = remaining,
        functions = this.functions.updated(name, FunctionDef(displayName, params, localCaptures.map(getVariableId), body)),
        variables =
          if localCaptures.contains(displayName) then this.variables.updated(id.value, this.variables(id.value).copy(boxxed = true))
          else this.variables
      )
    case _ => throw AssertionError("Tried to merge a block scope as a function scope")

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

object TypeContext:

  val default: TypeContext = TypeContext(
    scopes = Chunk(
      TypeScope.Block(
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
        variables = Map.empty
        /*variables = Map(
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
        )*/
      )
    ),
    functions = Map.empty,
    variables = Chunk.Indexed.empty
  )
  .declareVariableForce(Identifier("Unit"), Type.Unit)
  .declareVariableForce(Identifier("println"), Type.Fun(Chunk(Type.Any), Type.Unit))
  .declareVariableForce(Identifier("readInt"), Type.Fun(Chunk.empty, Type.Int))
  .declareVariableForce(Identifier("readFloat"), Type.Fun(Chunk.empty, Type.Float))
  .declareVariableForce(
    Identifier("length"),
    Type.TypeFun(
      typeParams = Chunk(Identifier("A")),
      output = Type.Fun(Chunk(Type.arrayOf(Type.Generic(Identifier("A")))), Type.Int)
    ),
  )
  .declareVariableForce(
    Identifier("get"),
    Type.TypeFun(
      typeParams = Chunk(Identifier("A")),
      output = Type.Fun(Chunk(Type.arrayOf(Type.Generic(Identifier("A"))), Type.Int), Type.Generic(Identifier("A")))
    )
  )
  .declareVariableForce(
    Identifier("Array"),
    Type.TypeFun(
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
    )
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

  def getVariable(name: Identifier): Option[(VariableId, Variable)] < Typing = modifyReturn(_.getVariable(name))

  def getVariableOrFail(name: Identifier): (VariableId, Variable) < Typing = modifyReturn(_.getVariableOrFail(name))

  def declareVariable(name: Identifier, variable: Variable): VariableId < Typing = modifyReturn(_.declareVariable(name, variable))

  def updateVariable(name: Identifier, variable: Variable): Unit < Typing = modify(_.updateVariable(name, variable))

  def getFunction(name: Identifier): Option[FunctionDef] < Typing = Var.use(_.getFunction(name))

  def newUniqueTypeName(name: Identifier): Identifier < Typing = Var.use(_.newUniqueTypeName(name))

  def newUniqueVarName(name: Identifier): Identifier < Typing = Var.use(_.newUniqueVarName(name))

  def newUniqueFunctionName(name: Identifier): Identifier < Typing = Var.use(_.newUniqueFunctionName(name))

  def inNewBlockScope[A](body: A < Typing): A < Typing =
    Var.isolate.merge[TypeContext](_.mergeBlock(_)).run(
      Var.update[TypeContext](ctx => ctx.copy(scopes = TypeScope.Block(Map.empty, Map.empty) +: ctx.scopes))
        .andThen(body)
    )

  def inNewFunctionScope(id: VariableId, displayName: Identifier, params: Chunk[Identifier])(body: Identifier => (Expr, Expr) < Typing): Expr < Typing = direct:
    val name = newUniqueFunctionName(displayName).now
    val ctx = Var.updateDiscard[TypeContext](ctx => ctx.copy(
      scopes = TypeScope.Function(id, Map.empty, Map.empty, Set.empty) +: ctx.scopes,
      functions = ctx.functions.updated(name, null) //Reserve name to avoid duplication of internal name
    )).now

    val (funBody, funDecl) = body(name).now
    Var.updateDiscard[TypeContext](_.popFunction(name, displayName, params, funBody)).now

    funDecl

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
