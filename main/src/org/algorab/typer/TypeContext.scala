package org.algorab.typer

import kyo.*
import org.algorab.ast.Identifier
import org.algorab.ast.tpd.Expr
import org.algorab.ast.tpd.Type
import scala.annotation.nowarn
import scala.annotation.tailrec

/*
val x = 5

class Foo:
  val y = x
 */

case class TypeContext(
    scopes: Chunk[TypeScope],
    functions: Map[Identifier, FunctionDef],
    classes: Map[Identifier, ClassTypeDef],
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

  def getVariable(name: Identifier): (TypeContext, Result[TypeFailure, (VariableId, Variable)]) =

    def isIllegalForwardReference(variable: Variable, captured: Boolean): Boolean =
      !variable.initialized && !variable.isFunDef

    @tailrec
    def rec(
        scopes: Chunk[TypeScope],
        updatedScopes: Chunk[TypeScope],
        captured: Boolean
    ): (Chunk[TypeScope], Result[TypeFailure, (VariableId, Variable)]) = scopes match
      case head +: tail =>
        head match
          case TypeScope.Block(_, variables) => variables.get(name) match
              case None => rec(tail, updatedScopes :+ head, captured)
              case Some(id) =>
                var variable = this.variables(id.value)
                if captured && variable.mutable then variable = variable.copy(boxxed = true)
                if isIllegalForwardReference(variable, captured) then
                  (updatedScopes ++ scopes, Result.fail(TypeFailure.IllegalForwardReference(name)))
                else
                  (updatedScopes ++ scopes, Result.succeed((id, variable)))
          case TypeScope.Function(fid, types, variables, captures) => variables.get(name) match
              case None => rec(tail, updatedScopes :+ TypeScope.Function(fid, types, variables, captures + name), true)
              case Some(id) =>
                var variable = this.variables(id.value)
                if captured && variable.mutable then variable = variable.copy(boxxed = true)
                if isIllegalForwardReference(variable, captured) then
                  (updatedScopes ++ scopes, Result.fail(TypeFailure.IllegalForwardReference(name)))
                else
                  (updatedScopes ++ scopes, Result.succeed((id, variable)))
          case TypeScope.Class(cid, types, variables, captures) => variables.get(name) match
              case None => rec(tail, updatedScopes :+ TypeScope.Class(cid, types, variables, captures + name), true)
              case Some(id) =>
                var variable = this.variables(id.value)
                if captured && variable.mutable then variable = variable.copy(boxxed = true)
                if isIllegalForwardReference(variable, captured) then
                  (updatedScopes ++ scopes, Result.fail(TypeFailure.IllegalForwardReference(name)))
                else
                  (updatedScopes ++ scopes, Result.succeed((id, variable)))

      case _ => (updatedScopes ++ scopes, Result.fail(TypeFailure.UnknownVariable(name)))

    val (updatedScopes, variable) = rec(scopes, Chunk.empty, false)
    variable match
      case Result.Success((id, v)) if v.boxxed =>
        val st = this.copy(variables = variables.updated(id.value, v), scopes = updatedScopes)
        (st, variable)
      case _ =>
        (this.copy(scopes = updatedScopes), variable)

  @nowarn("msg=exhaustive") // Because it is, actually.
  def getVariableOrFail(name: Identifier): (TypeContext, (VariableId, Variable)) < Typing =
    getVariable(name) match
      case (newCtx, Result.Success(value)) => (newCtx, value)
      case (_, Result.Failure(failure))    => Typing.failAndAbort(failure)
      case (_, Result.Panic(error))        => throw error

  def getVariableId(name: Identifier): VariableId =
    scopes.collectFirst(((scope: TypeScope) => scope.getVariable(name)).unlift).get

  def declareVariable(name: Identifier, variable: Variable): (TypeContext, VariableId) < Typing =
    scopes.head.getVariable(name) match
      case Some(_) => Typing.failAndAbort(TypeFailure.VariableAlreadyDefined(name))
      case None =>
        val id = VariableId.assume(variables.size)
        val field = scopes.head.isClassScope
        (
          this.copy(
            scopes = scopes.head.withVariable(name, id) +: scopes.tail,
            variables = variables :+ variable.copy(field = field)
          ),
          id
        )

  def declareVariableForce(name: Identifier, tpe: Type): TypeContext =
    this.copy(
      scopes = scopes.head.withVariable(name, VariableId.assume(variables.size)) +: scopes.tail,
      variables = variables :+ Variable(name, tpe, false, false, true)
    )

  def updateVariable(name: Identifier, variable: Variable): TypeContext =
    this.copy(
      variables = variables.updated(scopes.head.variables(name).value, variable.copy(field = scopes.head.isClassScope))
    )

  def getDeclarationOrFail(className: Identifier, memberName: Identifier): (VariableId, Variable) < Typing =
    classes.get(className).flatMap(_.declarations.get(memberName)) match
      case Some(decl) => (decl, this.variables(decl.value))
      case None       => Typing.failAndAbort(TypeFailure.UnknownMember(className, memberName))

  def mergeBlock(inner: TypeContext): TypeContext =
    this.copy(scopes = inner.scopes.drop(1), functions = inner.functions, variables = inner.variables)

  def popFunction(name: Identifier, displayName: Identifier, params: Chunk[Identifier], body: Expr): TypeContext = scopes match
    case TypeScope.Function(id, types, _, localCaptures) +: remaining =>
      var hasField = false
      var globalCaptures = localCaptures
        .map(getVariableId)
        .filterNot: id =>
          val isField = variables(id.value).field
          if isField then hasField = true
          isField

      if hasField then globalCaptures = globalCaptures + getVariableId(Identifier("this"))

      val (hasForwardCapture, updatedVariables) = globalCaptures.foldLeft((false, this.variables)):
        case ((hasFC, variables), id) =>
          val variable = variables(id.value)
          if variable.initialized then (hasFC, variables)
          else (true, variables.updated(id.value, variable.copy(boxxed = true)))

      val declaringVariable = updatedVariables(id.value).copy(functionId = Present(name))
      val declaringBoxxed =
        if hasForwardCapture then declaringVariable.copy(boxxed = true)
        else declaringVariable

      this.copy(
        scopes = remaining,
        functions = this.functions.updated(name, FunctionDef(displayName, params, globalCaptures, body, id)),
        variables = updatedVariables.updated(id.value, declaringBoxxed)
      )
    case scope +: _ => throw AssertionError(s"Tried to merge a ${scope.getClass} as a function scope")
    case _          => throw AssertionError("Tried to merge non-existing function scope")

  def popClass(name: Identifier, displayName: Identifier, parameters: Chunk[Identifier], init: Chunk[Expr]): TypeContext = scopes match
    case TypeScope.Class(id, types, variables, localCaptures) +: remaining =>
      val globalCaptures = localCaptures.map(getVariableId)
      val declaringVariable = this.variables(id.value).copy(initialized = true, classId = Present(name))
      this.copy(
        scopes = remaining,
        classes = this.classes.updated(name, ClassTypeDef(displayName, variables, parameters, globalCaptures, init, id)),
        variables = this.variables.updated(id.value, declaringVariable)
      )
    case scope +: _ => throw AssertionError(s"Tried to merge a ${scope.getClass} as a class scope")
    case _          => throw AssertionError("Tried to merge non-existing class scope")

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
      )
    ),
    functions = Map.empty,
    classes = Map.empty,
    variables = Chunk.Indexed.empty
  )
    .declareVariableForce(Identifier("Unit"), Type.Unit)
    .declareVariableForce(Identifier("println"), Type.Fun(Chunk(Type.Any), Type.Unit))
    .declareVariableForce(Identifier("readInt"), Type.Fun(Chunk.empty, Type.Int))
    .declareVariableForce(Identifier("readFloat"), Type.Fun(Chunk.empty, Type.Float))
    .declareVariableForce(Identifier("toFloat"), Type.Fun(Chunk(Type.Int), Type.Float))
    .declareVariableForce(
      Identifier("length"),
      Type.TypeFun(
        typeParams = Chunk(Identifier("A")),
        output = Type.Fun(Chunk(Type.arrayOf(Type.Generic(Identifier("A")))), Type.Int)
      )
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

  def getVariableOrFail(name: Identifier): (VariableId, Variable) < Typing = modifyReturn(_.getVariableOrFail(name))

  def declareVariable(name: Identifier, variable: Variable): VariableId < Typing = modifyReturn(_.declareVariable(name, variable))

  def updateVariable(name: Identifier, variable: Variable): Unit < Typing = modify(_.updateVariable(name, variable))

  def getDeclarationOrFail(className: Identifier, memberName: Identifier): (VariableId, Variable) < Typing =
    Var.use(_.getDeclarationOrFail(className, memberName))

  def newUniqueTypeName(name: Identifier): Identifier < Typing = Var.use(_.newUniqueTypeName(name))

  def newUniqueVarName(name: Identifier): Identifier < Typing = Var.use(_.newUniqueVarName(name))

  def newUniqueFunctionName(name: Identifier): Identifier < Typing = Var.use(_.newUniqueFunctionName(name))

  def inNewBlockScope[A](body: A < Typing): A < Typing =
    Var.isolate.merge[TypeContext](_.mergeBlock(_)).run(
      Var.update[TypeContext](ctx => ctx.copy(scopes = TypeScope.Block(Map.empty, Map.empty) +: ctx.scopes))
        .andThen(body)
    )

  /*
    - Create new function name
    - Reserve function name in context to avoid duplicate internal names for nested functions
    - Create new function scope with empty captures and variables
    - Run body to get function declaration and body
    - Pop function scope, updating captures and variables as needed
   */
  def inNewFunctionScope(
      id: VariableId,
      displayName: Identifier,
      params: Chunk[Identifier]
  )(body: Identifier => (Expr, Expr) < Typing): Expr < Typing = direct:
    val name = newUniqueFunctionName(displayName).now
    val ctx = Var.updateDiscard[TypeContext](ctx =>
      ctx.copy(
        scopes = TypeScope.Function(id, Map.empty, Map.empty, Set.empty) +: ctx.scopes,
        functions = ctx.functions.updated(name, null) // Reserve name to avoid duplication of internal name
      )
    ).now

    val (funBody, funDecl) = body(name).now
    Var.updateDiscard[TypeContext](_.popFunction(name, displayName, params, funBody)).now

    funDecl

  def inNewClassScope(
      id: VariableId,
      displayName: Identifier,
      parameters: Chunk[Identifier]
  )(body: Identifier => (Chunk[Expr], Expr) < Typing): Expr < Typing = direct:
    val name = newUniqueTypeName(displayName).now
    val ctx = Var.updateDiscard[TypeContext](ctx =>
      ctx.copy(
        scopes = TypeScope.Class(
          id = id,
          types = Map(name -> Type.Instance(name, Map.empty)),
          variables = Map.empty,
          captures = Set.empty
        ) +: ctx.scopes,
        classes = ctx.classes.updated(name, null) // Reserve name to avoid duplication of internal name
      ).declareVariableForce(Identifier("this"), Type.Instance(name, Map.empty))
    ).now

    val (classBody, classDecl) = body(name).now
    Var.updateDiscard[TypeContext](_.popClass(name, displayName, parameters, classBody)).now

    classDecl

  def isSubtype(tpe: Type, expected: Type): Boolean < Typing = (tpe, expected) match
    case (_, Type.Any)      => true
    case (_, Type.Inferred) => true
    case _                  => tpe == expected

  def union(typeA: Type, typeB: Type): Type < Typing = (typeA, typeB) match
    case (Type.Nothing, _)      => typeB
    case (_, Type.Nothing)      => typeA
    case (Type.Int, Type.Float) => Type.Float
    case _ =>
      if typeA == typeB then typeA
      else Type.Any
