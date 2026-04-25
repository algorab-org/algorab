/**
 * The type-checker's mutable environment.
 *
 * [[TypeContext]] is the central data structure of the type-checking phase.  It holds:
 *   - A scope stack (`scopes`) – the chain of [[TypeScope]]s currently in view, innermost first.
 *   - A function table (`functions`) – all `def`s typed so far, keyed by their internal name.
 *   - A class table (`classes`) – all `class`es typed so far, keyed by their internal name.
 *   - A flat variable array (`variables`) – indexed by [[VariableId]]; stores [[Variable]]
 *     metadata for every variable ever declared.
 *
 * The companion object [[TypeContext$]] provides a static effect-style API that operates
 * through `Var[TypeContext]` without callers needing to read/modify/write the state by hand.
 *
 * === Name uniqueness ===
 *
 * When a function or type is defined inside another function or loop, the typer assigns a
 * unique internal name (e.g. `foo$1`) via [[newUniqueFunctionName]] / [[newUniqueTypeName]]
 * to avoid clashing with names from outer scopes that happen to be the same.
 */
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

/**
 * Immutable snapshot of the complete type-checking environment.
 *
 * @param scopes    the scope chain, innermost first
 * @param functions the global function table; populated as `def`s are processed
 * @param classes   the global class table; populated as `class`es are processed
 * @param variables the flat variable metadata array, indexed by [[VariableId]]
 */
case class TypeContext(
    scopes: Chunk[TypeScope],
    functions: Map[Identifier, FunctionDef],
    classes: Map[Identifier, ClassTypeDef],
    variables: Chunk[Variable]
):

  /**
   * Returns the type bound to `name` by scanning the scope chain from innermost to outermost.
   *
   * @param name the type name to look up
   * @return `Some(tpe)` if found, `None` otherwise
   */
  def getType(name: Identifier): Option[Type] =
    scopes.collectFirst[Type](((scope: TypeScope) => scope.getType(name)).unlift)

  /**
   * Returns the type bound to `name`, or fails with [[TypeFailure.UnknownType]].
   *
   * @param name the type name to resolve
   * @return the resolved type, effectful in [[Typing]]
   */
  def getTypeOrFail(name: Identifier): Type < Typing =
    getType(name) match
      case Some(value) => value
      case None        => Typing.failAndAbort(TypeFailure.UnknownType(name))

  /**
   * Adds a new type binding to the innermost scope, or fails if the name is already declared.
   *
   * @param name the type name to bind
   * @param tpe  the type to associate with `name`
   * @return the updated [[TypeContext]], effectful in [[Typing]]
   */
  def declareType(name: Identifier, tpe: Type): TypeContext < Typing =
    scopes.head.getType(name) match
      case Some(_) => Typing.failAndAbort(TypeFailure.TypeAlreadyDefined(name))
      case None    => updateType(name, tpe)

  /**
   * Unconditionally updates or adds a type binding in the innermost scope.
   *
   * @param name the type name to update
   * @param tpe  the new type value
   * @return the updated [[TypeContext]], effectful in [[Typing]]
   */
  def updateType(name: Identifier, tpe: Type): TypeContext < Typing =
    this.copy(scopes = scopes.head.withType(name, tpe) +: scopes.tail)

  /**
   * Looks up a variable by name, performing capture analysis along the way.
   *
   * The lookup traverses the scope chain from innermost to outermost:
   *   - In `Block` scopes, the variable is found or the search continues outward.
   *   - In `Function` scopes, if the variable is found in an *outer* scope the name is
   *     added to the function's capture set.
   *   - In `Class` scopes, if the variable is not found locally the lookup stops and returns
   *     [[TypeFailure.UnknownVariable]] (class scopes are opaque to outer captures).
   *
   * If a mutable variable is accessed across a function boundary, it is marked `boxxed = true`
   * so the compiler will wrap it in a `VBox` to allow shared mutation between the outer scope
   * and the closure.
   *
   * @param name the variable to look up
   * @return `(updatedContext, Result)` – the context is updated if boxing was required
   */
  def getVariable(name: Identifier): (TypeContext, Result[TypeFailure, (VariableId, Variable)]) =

    def isIllegalForwardReference(variable: Variable, captured: Boolean): Boolean =
      !variable.initialized && !variable.isFunDef && !variable.isClassDef

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

  /**
   * Looks up a variable by name, or fails with the appropriate [[TypeFailure]].
   *
   * @param name the variable to look up
   * @return `(updatedContext, (id, variable))`, effectful in [[Typing]]
   */
  @nowarn("msg=exhaustive") // Because it is, actually.
  def getVariableOrFail(name: Identifier): (TypeContext, (VariableId, Variable)) < Typing =
    getVariable(name) match
      case (newCtx, Result.Success(value)) => (newCtx, value)
      case (_, Result.Failure(failure))    => Typing.failAndAbort(failure)
      case (_, Result.Panic(error))        => throw error

  /**
   * Returns the [[VariableId]] for `name` from the current scope chain (no capture analysis).
   *
   * Used internally when the variable is known to be in scope.
   *
   * @param name the variable name
   * @return the corresponding [[VariableId]]
   */
  def getVariableId(name: Identifier): VariableId =
    scopes.collectFirst(((scope: TypeScope) => scope.getVariable(name)).unlift).get

  /**
   * Declares a new variable in the innermost scope, or fails if the name is already declared.
   *
   * The variable is marked as a field if the innermost scope is a `Class` scope.
   *
   * @param name     the variable name
   * @param variable the initial metadata (type may be [[Type.Inferred]])
   * @return `(updatedContext, newId)`, effectful in [[Typing]]
   */
  def declareVariable(name: Identifier, variable: Variable): (TypeContext, VariableId) < Typing =
    scopes.head.getVariable(name) match
      case Some(_) => Typing.failAndAbort(TypeFailure.VariableAlreadyDefined(name))
      case None =>
        val id = VariableId.assume(variables.size)
        scopes.head match
          case scope @ TypeScope.Class(classVarId, _, _, _) =>
            val cid = variables(classVarId.value).classId.get
            val classDef = classes(cid)
            (
              this.copy(
                scopes = scope.withVariable(name, id) +: scopes.tail,
                variables = variables :+ variable.copy(field = true),
                classes = classes.updated(cid, classDef.copy(declarations = classDef.declarations.updated(name, id)))
              ),
              id
            )
          case scope =>
            (
              this.copy(
                scopes = scope.withVariable(name, id) +: scopes.tail,
                variables = variables :+ variable.copy(field = scope.isClassScope)
              ),
              id
            )

  /**
   * Declares a variable unconditionally (no duplicate check), used for built-in pre-declarations.
   *
   * @param name the variable name
   * @param tpe  the variable's type
   * @return the updated [[TypeContext]]
   */
  def declareVariableForce(name: Identifier, tpe: Type): TypeContext =
    this.copy(
      scopes = scopes.head.withVariable(name, VariableId.assume(variables.size)) +: scopes.tail,
      variables = variables :+ Variable(name, tpe, false, false, true)
    )

  /**
   * Replaces the metadata for the variable `name` in the innermost scope with `variable`.
   *
   * @param name     the variable to update
   * @param variable the new metadata
   * @return the updated [[TypeContext]]
   */
  def updateVariable(name: Identifier, variable: Variable): TypeContext =
    this.copy(
      variables = variables.updated(scopes.head.variables(name).value, variable.copy(field = scopes.head.isClassScope))
    )

  /**
   * Returns the declaration metadata for a class field, or fails if not found.
   *
   * @param className  the class whose member is being accessed
   * @param memberName the member name to look up
   * @return `(id, variable)`, effectful in [[Typing]]
   */
  def getDeclarationOrFail(className: Identifier, memberName: Identifier): (VariableId, Variable) < Typing =
    classes.get(className).flatMap(_.declarations.get(memberName)) match
      case Some(decl) => (decl, this.variables(decl.value))
      case None       => Typing.failAndAbort(TypeFailure.UnknownMember(className, memberName))

  /**
   * Merges an inner block context back into `this` after the block has been type-checked.
   *
   * Drops the innermost scope (which belongs to the block) and adopts the function/class
   * tables and variable array from the inner context.
   *
   * @param inner the context after the block was typed
   * @return the merged context
   */
  def mergeBlock(inner: TypeContext): TypeContext =
    this.copy(scopes = inner.scopes.drop(1), functions = inner.functions, variables = inner.variables)

  /**
   * Pops a function scope, extracts capture information, and records the [[FunctionDef]].
   *
   * After the function body has been typed, this method:
   *   1. Resolves the global capture set (converting local capture names to [[VariableId]]s,
   *      filtering out field accesses, and adding `this` if any field is captured).
   *   1. Marks any not-yet-initialised captured variables as `boxxed` (forward closure capture).
   *   1. Records the final [[FunctionDef]] in `this.functions`.
   *
   * @param name        the internal function name
   * @param displayName the source-level function name
   * @param params      the parameter names
   * @param body        the typed body expression
   * @return the updated [[TypeContext]] with the function scope removed
   */
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

  /**
   * Pops a class scope and records the [[ClassTypeDef]].
   *
   * @param name        the internal class name
   * @param displayName the source-level class name
   * @param parameters  the constructor parameter names
   * @param init        the typed constructor body expressions
   * @return the updated [[TypeContext]] with the class scope removed
   */
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

  /**
   * Returns a unique internal type name derived from `baseName`.
   *
   * Scans all type bindings across all scopes and appends `$N` (incrementing N) if
   * `baseName` is already taken.  If not taken, returns `baseName` unchanged.
   *
   * @param baseName the desired base name
   * @return `baseName` or `baseName$N` for some N ≥ 1
   */
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

  /**
   * Returns a unique internal variable name derived from `baseName`.
   *
   * Scans all variable bindings across all scopes.
   *
   * @param baseName the desired base name
   * @return `baseName` or `baseName$N` for some N ≥ 1
   */
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

  /**
   * Returns a unique internal function name derived from `baseName`.
   *
   * Scans the function table (not the scope chain).
   *
   * @param baseName the desired base name
   * @return `baseName` or `baseName$N` for some N ≥ 1
   */
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

/**
 * Companion containing the default context and the static effect-style API.
 *
 * All methods here operate through `Var[TypeContext]` (part of the [[Typing]] effect)
 * so callers do not need to read-modify-write the state by hand.
 */
object TypeContext:

  /** The default starting context, pre-populated with the built-in types and functions. */
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

  /** Applies `f` to the current context and replaces it with the returned context. */
  def modify(f: TypeContext => TypeContext < Typing): Unit < Typing =
    Var.use[TypeContext](f)
      .map(Var.set)
      .unit

  /**
   * Applies `f` to the current context, returning the first component and updating the context
   * with the second component.
   */
  def modifyReturn[A](f: TypeContext => (TypeContext, A) < Typing): A < Typing =
    Var.use[TypeContext](f)
      .map(Var.set(_).andThen(_))

  /** Returns the type bound to `name` in the current context. */
  def getType(name: Identifier): Option[Type] < Typing = Var.use(_.getType(name))

  /** Returns the type bound to `name`, or fails with [[TypeFailure.UnknownType]]. */
  def getTypeOrFail(name: Identifier): Type < Typing = Var.use(_.getTypeOrFail(name))

  /** Adds a new type binding to the innermost scope of the current context. */
  def declareType(name: Identifier, tpe: Type): Unit < Typing = modify(_.declareType(name, tpe))

  /** Unconditionally updates or adds a type binding in the innermost scope. */
  def updateType(name: Identifier, tpe: Type): Unit < Typing = modify(_.updateType(name, tpe))

  /** Looks up a variable by name in the current context, performing capture analysis. */
  def getVariableOrFail(name: Identifier): (VariableId, Variable) < Typing = modifyReturn(_.getVariableOrFail(name))

  /** Declares a new variable in the innermost scope of the current context. */
  def declareVariable(name: Identifier, variable: Variable): VariableId < Typing = modifyReturn(_.declareVariable(name, variable))

  /** Replaces the metadata for `name` in the current context. */
  def updateVariable(name: Identifier, variable: Variable): Unit < Typing = modify(_.updateVariable(name, variable))

  /** Returns the declaration metadata for a class field in the current context. */
  def getDeclarationOrFail(className: Identifier, memberName: Identifier): (VariableId, Variable) < Typing =
    Var.use(_.getDeclarationOrFail(className, memberName))

  /** Returns a unique type name derived from `name`. */
  def newUniqueTypeName(name: Identifier): Identifier < Typing = Var.use(_.newUniqueTypeName(name))

  /** Returns a unique variable name derived from `name`. */
  def newUniqueVarName(name: Identifier): Identifier < Typing = Var.use(_.newUniqueVarName(name))

  /** Returns a unique function name derived from `name`. */
  def newUniqueFunctionName(name: Identifier): Identifier < Typing = Var.use(_.newUniqueFunctionName(name))

  /**
   * Runs `body` inside a fresh `Block` scope, then merges the block back into the parent.
   *
   * Variables declared inside the block are invisible after it ends.
   *
   * @param body the computation to run in the new block scope
   * @tparam A the result type
   */
  def inNewBlockScope[A](body: A < Typing): A < Typing =
    Var.isolate.merge[TypeContext](_.mergeBlock(_)).run(
      Var.update[TypeContext](ctx => ctx.copy(scopes = TypeScope.Block(Map.empty, Map.empty) +: ctx.scopes))
        .andThen(body)
    )

  /**
   * Runs `body` inside a fresh `Function` scope, then pops the scope and records the function.
   *
   * Steps performed:
   *   1. Allocates a unique internal name for the function.
   *   1. Pushes a new [[TypeScope.Function]] onto the scope chain.
   *   1. Calls `body(internalName)`, which should return `(typedBody, declaration)`.
   *   1. Pops the function scope via [[TypeContext.popFunction]], recording capture information.
   *   1. Returns `declaration` (the `Assign` node that initialises the function variable).
   *
   * @param id          the [[VariableId]] allocated for the function variable
   * @param displayName the source-level function name
   * @param params      the parameter names
   * @param body        a function from the internal name to `(typedBody, declarationExpr)`
   * @return the declaration expression for the function
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

  /**
   * Runs `body` inside a fresh `Class` scope, then pops the scope and records the class.
   *
   * Steps performed:
   *   1. Allocates a unique internal name for the class.
   *   1. Pushes a new [[TypeScope.Class]] and declares `this`.
   *   1. Calls `body(internalName)`, which returns `(classBodyExprs, declarationExpr)`.
   *   1. Pops the class scope via [[TypeContext.popClass]].
   *   1. Returns `declarationExpr` (the `Assign` node that initialises the class variable).
   *
   * @param id          the [[VariableId]] allocated for the class constructor variable
   * @param displayName the source-level class name
   * @param parameters  the constructor parameter names
   * @param body        a function from the internal name to `(initExprs, declarationExpr)`
   * @return the declaration expression for the class
   */
  def inNewClassScope(
      id: VariableId,
      displayName: Identifier,
      typeParams: Chunk[Identifier],
      parameters: Chunk[Identifier]
  )(body: Identifier => (Chunk[Expr], Expr) < Typing): Expr < Typing = direct:
    val name = Var.use[TypeContext](_.variables(id.value).classId.get).now
    val ctx = Var.updateDiscard[TypeContext](ctx =>
      ctx.copy(
        scopes = TypeScope.Class(
          id = id,
          types = Map(name -> Type.Instance(name, typeParams, typeParams.map(name => (name, Type.Generic(name))).toMap)),
          variables = Map.empty,
          captures = Set.empty
        ) +: ctx.scopes,
        classes =
          ctx.classes.updated(
            name,
            ClassTypeDef(displayName, Map.empty, parameters, Set.empty, Chunk.empty, id)
          ) // Reserve name to avoid duplication of internal name
      ).declareVariableForce(Identifier("this"), Type.Instance(name, typeParams, typeParams.map(name => (name, Type.Generic(name))).toMap))
    ).now

    val (classBody, classDecl) = body(name).now
    Var.updateDiscard[TypeContext](_.popClass(name, displayName, parameters, classBody)).now

    classDecl

  /**
   * Returns `true` iff `tpe` is a subtype of `expected` under the current type context.
   *
   * Currently implements a simple type equality check extended with:
   *   - `Any` accepts everything.
   *   - `Inferred` accepts everything (for use in partial type information).
   *
   * @param tpe      the type to check
   * @param expected the expected supertype
   */
  def isSubtype(tpe: Type, expected: Type): Boolean < Typing = (tpe, expected) match
    case (_, Type.Any)      => true
    case (_, Type.Inferred) => true
    case _                  => tpe == expected

  /**
   * Computes the least upper bound of two types.
   *
   * Rules:
   *   - `Nothing` is the neutral element (identity under union).
   *   - `Int ∪ Float = Float` (widening).
   *   - Equal types return the same type.
   *   - All other combinations return `Any`.
   *
   * @param typeA the first type
   * @param typeB the second type
   * @return the least upper bound
   */
  def union(typeA: Type, typeB: Type): Type < Typing = (typeA, typeB) match
    case (Type.Nothing, _)      => typeB
    case (_, Type.Nothing)      => typeA
    case (Type.Int, Type.Float) => Type.Float
    case _ =>
      if typeA == typeB then typeA
      else Type.Any
