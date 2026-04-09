/** The Algorab type-checker.
  *
  * [[Typer]] is the main type-checking pass.  It takes an [[org.algorab.ast.untpd.Expr]] tree
  * produced by the parser and:
  *   1. Resolves untyped [[org.algorab.ast.untpd.Type]] references to their [[org.algorab.ast.tpd.Type]]
  *      equivalents.
  *   1. Assigns a [[org.algorab.ast.tpd.Type]] to every expression node, producing a
  *      [[org.algorab.ast.tpd.Expr]] tree.
  *   1. Reports [[TypeFailure]]s for any inconsistencies found.
  *   1. Performs capture analysis for closures and marks mutable captured variables as `boxxed`.
  *   1. Detects and boxes cyclic closures (functions that capture a reference to themselves,
  *      directly or transitively) so that the compiler can emit correct code.
  *
  * The main entry point is [[typeProgram]], which also applies the [[boxCyclicClosures]] post-pass
  * to the finished context before returning.
  */
package org.algorab.typer

import kyo.*
import org.algorab.assertionError
import org.algorab.ast.Identifier
import org.algorab.ast.tpd
import org.algorab.ast.untpd
import scala.annotation.meta.param
import scala.languageFeature.experimental.macros

/** Type-checks the untyped AST, producing an annotated typed AST. */
object Typer:

  /** Asserts that `tpe` is a subtype of at least one of `expected`.
    *
    * Emits [[TypeFailure.Mismatch]] and does NOT abort if the check fails, allowing the
    * caller to continue and report further errors.
    *
    * @param tpe      the type to check
    * @param expected the acceptable supertypes
    */
  def assertSubtype(tpe: tpd.Type, expected: tpd.Type*): Unit < Typing = direct:
    if expected.exists(TypeContext.isSubtype(tpe, _).now) then ()
    else Typing.fail(TypeFailure.Mismatch(tpe, expected*)).now

  /** Attempts to cast `expr` to one of the `expected` types.
    *
    * - If `expr`'s type is already a subtype of one of the expected types, returns
    *   `Present(expr)` unchanged.
    * - If `expr` has type `Int` and the expected type is `Float`, inserts a `toFloat` call
    *   to perform implicit widening.
    * - Otherwise returns `Absent`.
    *
    * @param expr     the typed expression to cast
    * @param expected the acceptable target types
    * @return `Present(castedExpr)` on success, `Absent` on failure
    */
  def cast(expr: tpd.Expr, expected: tpd.Type*): Maybe[tpd.Expr] < Typing =
    Kyo.findFirst(expected)(tpe =>
      TypeContext.isSubtype(expr.exprType, tpe).map(isSub =>
        if isSub then
          Present(expr)
        else if expr.exprType == tpd.Type.Int && tpe == tpd.Type.Float then
          TypeContext
            .getVariableOrFail(Identifier("toFloat"))
            .map((toFloatId, variable) =>
              Present(tpd.Expr.Apply(
                expr = tpd.Expr.VarCall(toFloatId, Identifier("toFloat"), variable.tpe),
                args = Chunk(expr),
                exprType = tpd.Type.Float
              ))
            )
        else
          Absent
      )
    )

  /** Casts `expr` to one of the `expected` types, or fails with [[TypeFailure.Mismatch]].
    *
    * @param expr     the typed expression to cast
    * @param expected the acceptable target types
    * @return the (possibly coerced) expression
    */
  def castOrFail(expr: tpd.Expr, expected: tpd.Type*): tpd.Expr < Typing =
    cast(expr, expected*).map:
      case Absent =>
        Typing.failAndAbort(TypeFailure.Mismatch(expr.exprType, expected*))
      case Present(finalExpr) => finalExpr

  /** Types and casts both operands of a binary operator, expecting each to satisfy `expected`.
    *
    * @param left     the left-hand source expression
    * @param right    the right-hand source expression
    * @param expected the acceptable operand types
    * @param op       a function that combines the typed operands into the result expression
    * @return the typed result expression
    */
  def assertBinaryOp(left: untpd.Expr, right: untpd.Expr, expected: tpd.Type*)(op: (tpd.Expr, tpd.Expr) => tpd.Expr): tpd.Expr < Typing = direct:
    op(typeAndCast(left, expected*).now, typeAndCast(right, expected*).now)

  /** Types both operands and verifies that they satisfy one of the expected `(inputType, outputType)` pairs.
    *
    * Used for operators like `+` that are polymorphic but have a fixed output type per input type.
    *
    * @param left     the left-hand source expression
    * @param right    the right-hand source expression
    * @param expected pairs of `(operandType, resultType)` that are acceptable
    * @param op       a function combining the typed operands and the chosen result type
    * @return the typed result expression
    */
  def assertDependentBinaryOp(
      left: untpd.Expr,
      right: untpd.Expr
  )(expected: (tpd.Type, tpd.Type)*)(op: (tpd.Expr, tpd.Expr, tpd.Type) => tpd.Expr): tpd.Expr < Typing = direct:
    val typedLeft = typeExpr(left).now
    val typedRight = typeExpr(right).now

    val result = Kyo.findFirst(expected)(tpl =>
      for
        leftCast <- cast(typedLeft, tpl._1)
        rightCast <- cast(typedRight, tpl._1)
      yield (leftCast, rightCast) match
        case (Present(l), Present(r)) => Present((l, r, tpl._2))
        case _                        => Absent
    ).now

    result match
      case Present((leftCast, rightCast, resultType)) => op(leftCast, rightCast, resultType)
      case Absent =>
        val operandTypes = typedLeft.exprType.zip(typedRight.exprType)
        val expectedTypes = expected.map(tpe => tpe._1.zip(tpe._1))
        Typing.failAndAbort(TypeFailure.Mismatch(operandTypes, expectedTypes*)).now

  /** Types and casts both operands to a numeric type, producing a Boolean result.
    *
    * Used for `<`, `<=`, `>`, `>=`.
    *
    * @param op a function combining typed operands and the Boolean result type
    */
  def assertComparison(left: untpd.Expr, right: untpd.Expr)(op: (tpd.Expr, tpd.Expr, tpd.Type) => tpd.Expr): tpd.Expr < Typing =
    assertBinaryOp(left, right, tpd.Type.Int, tpd.Type.Float)(op(_, _, tpd.Type.Boolean))

  /** Types and casts both operands to Boolean, producing a Boolean result.
    *
    * Used for `and`, `or`.
    *
    * @param op a function combining typed operands and the Boolean result type
    */
  def assertBooleanOp(left: untpd.Expr, right: untpd.Expr)(op: (tpd.Expr, tpd.Expr, tpd.Type) => tpd.Expr): tpd.Expr < Typing =
    assertBinaryOp(left, right, tpd.Type.Boolean)(op(_, _, tpd.Type.Boolean))

  /** Types `expr` and casts the result to one of `expected`, failing if not possible.
    *
    * @param expr     the source expression to type and cast
    * @param expected the acceptable result types
    * @return the typed and cast expression
    */
  def typeAndCast(expr: untpd.Expr, expected: tpd.Type*): tpd.Expr < Typing =
    typeExpr(expr).map(castOrFail(_, expected*))

  /** Types two expressions and ensures their types are mutually castable.
    *
    * Tries to cast A to B's type, then B to A's type.  Used for `==` and `!=` where both
    * sides must be "related" but not necessarily identical.
    *
    * @return `(typedA, typedB)` where one may have been coerced to match the other
    */
  def typeAndAssertRelated(exprA: untpd.Expr, exprB: untpd.Expr): (tpd.Expr, tpd.Expr) < Typing = direct:
    val typedA = typeExpr(exprA).now
    val typedB = typeExpr(exprB).now

    cast(typedA, typedB.exprType).now match
      case Present(castedA) => (castedA, typedB)
      case Absent => cast(typedB, typedA.exprType).now match
          case Present(castedB) => (typedA, castedB)
          case Absent           => Typing.failAndAbort(TypeFailure.Mismatch(typedA.exprType, typedB.exprType)).now

  /** Assigns unique internal names to type parameters and resolves parameter / return types.
    *
    * For each type parameter in `funDef.typeParams`, a unique internal name is generated and
    * declared in the current scope as a `Generic`.  All parameter types and the return type
    * are then resolved against this enriched scope.
    *
    * If the function has no type parameters the outer type is `Fun(paramTypes, retType)`;
    * otherwise it is `TypeFun(uniqueTypeParams, Fun(paramTypes, retType))`.
    *
    * @param funDef the function definition whose parameters are to be resolved
    * @return `(uniqueTypeParams, resolvedParams, paramTypes, resolvedRetType, outerFunType)`
    */
  def declareTypeParamsAndResolveFunTypes(funDef: untpd.Expr.FunDef): (
      Chunk[(Identifier, Identifier)],
      Chunk[(Identifier, tpd.Type)],
      Chunk[tpd.Type],
      tpd.Type,
      tpd.Type
  ) < Typing = direct:
    val uniqueTypeParams = funDef.typeParams.map(tp => (tp, TypeContext.newUniqueTypeName(tp).now))
    uniqueTypeParams.foreach((originalName, newName) =>
      TypeContext.declareType(originalName, tpd.Type.Generic(newName)).now
    )

    val resolvedParams = funDef.params.map((name, tpe) => (name, resolveType(tpe).now))
    val paramTypes = resolvedParams.map(_._2)
    val resolvedRetType = resolveType(funDef.retType).now

    if funDef.typeParams.isEmpty then (uniqueTypeParams, resolvedParams, paramTypes, resolvedRetType, tpd.Type.Fun(paramTypes, resolvedRetType))
    else
      (
        uniqueTypeParams,
        resolvedParams,
        paramTypes,
        resolvedRetType,
        tpd.Type.TypeFun(uniqueTypeParams.map(_._2), tpd.Type.Fun(paramTypes, resolvedRetType))
      )

  /** Converts an [[untpd.Type]] to a [[tpd.Type]] by resolving all name references.
    *
    * @param tpe the unresolved type
    * @return the resolved type, effectful in [[Typing]]
    */
  def resolveType(tpe: untpd.Type): tpd.Type < Typing = direct:
    tpe match
      case untpd.Type.Inferred => tpd.Type.Inferred
      case untpd.Type.Ref(name) => TypeContext.getType(name).now match
          case Some(resolvedType) => resolvedType
          case None               => Typing.failAndAbort(TypeFailure.UnknownType(name)).now
      case untpd.Type.Apply(base, args) =>
        val resolvedArgs = args.map(resolveType(_).now)
        resolveType(base).now match
          case tpd.Type.Instance(name, typeParams, replacements) => tpd.Type.Instance(name, typeParams, replacements ++ typeParams.zip(resolvedArgs))
          case resolvedBase                                      => tpd.Type.Apply(resolvedBase, resolvedArgs)
      case untpd.Type.Fun(params, output) =>
        tpd.Type.Fun(params.map(resolveType(_).now), resolveType(output).now)
      case untpd.Type.TypeFun(typeParams, output) =>
        tpd.Type.TypeFun(typeParams, resolveType(output).now)
      case untpd.Type.Tuple(elements) =>
        tpd.Type.Tuple(elements.map(resolveType(_).now))

  /** Post-pass that boxes variables involved in cyclic closures.
    *
    * A cyclic closure is a function that (directly or transitively) captures a reference to
    * itself.  For example, a recursive function `f` that is captured by a helper `g` which is
    * then captured by `f` again.  Without boxing, loading `f` would attempt to read a variable
    * that may not yet be initialised.
    *
    * The algorithm performs a DFS over the capture graph; any function encountered a second time
    * during the traversal is marked `boxxed`.
    *
    * @param context the finished type context after all declarations have been processed
    * @return the context with cyclic-closure variables boxed
    */
  def boxCyclicClosures(context: TypeContext): TypeContext =
    def rec(
        variables: Chunk[Variable],
        functions: Map[Identifier, FunctionDef],
        visited: Set[Identifier],
        funId: Identifier,
        funDef: FunctionDef
    ): (Chunk[Variable], Set[Identifier]) =
      if visited.contains(funId) then
        val variable = variables(funDef.varId.value)
        (variables.updated(funDef.varId.value, variable.copy(boxxed = true)), visited)
      else
        val capturedVariables = funDef.captures.map(id => variables(id.value))
        capturedVariables.foldLeft((variables, visited + funId)):
          case ((variables, visited), variable) =>
            variable.functionId match
              case Absent => (variables, visited)
              case Present(capturedFunId) =>
                rec(variables, functions, visited, capturedFunId, functions(capturedFunId))

    val updatedVariables = context.functions.foldLeft(context.variables):
      case (variables, (funId, funDef)) => rec(variables, context.functions, Set.empty, funId, funDef)._1

    context.copy(variables = updatedVariables)

  /** Pre-declares all top-level names in a block without typing their bodies.
    *
    * Required to support forward references within the same block (e.g. a function can call
    * another function declared later in the same block).  Only `ValDef`, `FunDef`, and
    * `ClassDef` expressions participate; other expressions produce an empty `Chunk`.
    *
    * @param expr a single expression from the block being pre-processed
    * @return the `(id, name)` pairs for any variables declared
    */
  def typeDeclaration(expr: untpd.Expr): Chunk[(VariableId, Identifier)] < Typing = direct:
    expr match
      case untpd.Expr.ValDef(name, tpe, _, mutable) =>
        val resolvedType = resolveType(tpe).now
        Chunk((
          TypeContext.declareVariable(name, Variable(name, resolvedType, mutable, false, false)).now,
          name
        ))
      case funDef: untpd.Expr.FunDef =>
        val (_, _, _, _, funType) = TypeContext.inNewBlockScope(declareTypeParamsAndResolveFunTypes(funDef)).now
        Chunk(
          (
            TypeContext.declareVariable(
              funDef.name,
              Variable(funDef.name, funType, false, false, false, functionId = Present(Identifier.assume(null)))
            ).now,
            funDef.name
          )
        )
      case untpd.Expr.ClassDef(name, typeParams, parameters, body) =>
        val internalName = TypeContext.newUniqueTypeName(name).now
        TypeContext.declareType(name, tpd.Type.Instance(internalName, typeParams, typeParams.map(name => (name, tpd.Type.Generic(name))).toMap)).now
        val (_, _, _, _, constructorType) = TypeContext.inNewBlockScope(declareTypeParamsAndResolveFunTypes(untpd.Expr.FunDef(
          name = Identifier.assume(s"<$name constructor>"),
          params = parameters,
          typeParams = typeParams,
          retType = untpd.Type.Ref(name),
          body = untpd.Expr.Block(body)
        ))).now

        Chunk((
          TypeContext.declareVariable(
            name,
            Variable(name, tpd.Type.Class(name, constructorType), false, false, false, classId = Present(internalName))
          ).now,
          name
        ))

      case _ =>
        Chunk.empty

  /** Types an entire program, returning the final [[TypeContext]] and the typed expression.
    *
    * Applies [[typeExpr]] to produce the typed tree, then runs [[boxCyclicClosures]] as a
    * post-pass on the resulting context.
    *
    * @param expr the top-level (block) expression
    * @return `(finalContext, typedExpr)`, effectful in [[Typing]]
    */
  def typeProgram(expr: untpd.Expr): (TypeContext, tpd.Expr) < Typing = direct:
    val typedExpr = typeExpr(expr).now
    (Var.use[TypeContext](boxCyclicClosures).now, typedExpr)

  /** Types a single Algorab expression.
    *
    * The method is the workhorse of the type-checker.  It handles every variant of
    * [[untpd.Expr]], producing the corresponding [[tpd.Expr]] with type annotations.
    *
    * Notable translation rules:
    *   - `untpd.ValDef` → pre-declare the variable, type the initialiser, then emit `Assign`.
    *   - `untpd.FunDef` → run in a new function scope, collect captures, emit `Assign(FunRef)`.
    *   - `untpd.ClassDef` → run in a new class scope, collect declarations, emit `Assign(ClassRef)`.
    *   - `untpd.TypeApply` → resolve type arguments and bake them into the expression type.
    *   - `untpd.For` → desugar to a `While` loop with an index variable.
    *   - `untpd.Block` → pre-declare all top-level names, then type each expression in order.
    *
    * @param expr the untyped expression to type-check
    * @return the corresponding typed expression, effectful in [[Typing]]
    */
  def typeExpr(expr: untpd.Expr): tpd.Expr < Typing = direct:
    expr match
      case untpd.Expr.LBool(value)   => tpd.Expr.LBool(value, tpd.Type.Boolean)
      case untpd.Expr.LInt(value)    => tpd.Expr.LInt(value, tpd.Type.Int)
      case untpd.Expr.LFloat(value)  => tpd.Expr.LFloat(value, tpd.Type.Float)
      case untpd.Expr.LChar(value)   => tpd.Expr.LChar(value, tpd.Type.Char)
      case untpd.Expr.LString(value) => tpd.Expr.LString(value, tpd.Type.String)
      case untpd.Expr.Not(expr)      => tpd.Expr.Not(typeAndCast(expr, tpd.Type.Boolean).now, tpd.Type.Boolean)
      case untpd.Expr.Equal(left, right) =>
        val (typedLeft, typedRight) = typeAndAssertRelated(left, right).now
        tpd.Expr.Equal(typedLeft, typedRight, tpd.Type.Boolean)
      case untpd.Expr.NotEqual(left, right) =>
        val (typedLeft, typedRight) = typeAndAssertRelated(left, right).now
        tpd.Expr.NotEqual(typedLeft, typedRight, tpd.Type.Boolean)
      case untpd.Expr.Less(left, right) =>
        assertComparison(left, right)(tpd.Expr.Less.apply).now
      case untpd.Expr.LessEqual(left, right) =>
        assertComparison(left, right)(tpd.Expr.LessEqual.apply).now
      case untpd.Expr.Greater(left, right) =>
        assertComparison(left, right)(tpd.Expr.Greater.apply).now
      case untpd.Expr.GreaterEqual(left, right) =>
        assertComparison(left, right)(tpd.Expr.GreaterEqual.apply).now
      case untpd.Expr.Plus(expr) =>
        typeAndCast(expr, tpd.Type.Int, tpd.Type.Float).now
      case untpd.Expr.Minus(expr) =>
        val typedExpr = typeExpr(expr).now
        typedExpr.exprType match
          case tpd.Type.Int   => tpd.Expr.Minus(typedExpr, tpd.Type.Int)
          case tpd.Type.Float => tpd.Expr.Minus(typedExpr, tpd.Type.Float)
          case _              => Typing.failAndAbort(TypeFailure.Mismatch(typedExpr.exprType, tpd.Type.Int, tpd.Type.Float)).now

      case untpd.Expr.Add(left, right) =>
        assertDependentBinaryOp(left, right)(
          tpd.Type.Int -> tpd.Type.Int,
          tpd.Type.Float -> tpd.Type.Float,
          tpd.Type.String -> tpd.Type.String
        )(tpd.Expr.Add.apply).now
      case untpd.Expr.Sub(left, right) =>
        assertDependentBinaryOp(left, right)(
          tpd.Type.Int -> tpd.Type.Int,
          tpd.Type.Float -> tpd.Type.Float
        )(tpd.Expr.Sub.apply).now
      case untpd.Expr.Mul(left, right) =>
        assertDependentBinaryOp(left, right)(
          tpd.Type.Int -> tpd.Type.Int,
          tpd.Type.Float -> tpd.Type.Float
        )(tpd.Expr.Mul.apply).now
      case untpd.Expr.Div(left, right) =>
        assertBinaryOp(left, right, tpd.Type.Int, tpd.Type.Float)(tpd.Expr.Div(_, _, tpd.Type.Float)).now
      case untpd.Expr.IntDiv(left, right) =>
        assertBinaryOp(left, right, tpd.Type.Int, tpd.Type.Float)(tpd.Expr.IntDiv(_, _, tpd.Type.Int)).now
      case untpd.Expr.Mod(left, right) =>
        assertBinaryOp(left, right, tpd.Type.Int, tpd.Type.Float)(tpd.Expr.Mod(_, _, tpd.Type.Int)).now
      case untpd.Expr.And(left, right) =>
        assertBooleanOp(left, right)(tpd.Expr.And.apply).now
      case untpd.Expr.Or(left, right) =>
        assertBooleanOp(left, right)(tpd.Expr.Or.apply).now
      case untpd.Expr.VarCall(name) =>
        val (id, variable) = TypeContext.getVariableOrFail(name).now
        tpd.Expr.VarCall(id, name, variable.tpe)
      case untpd.Expr.ValDef(name, tpe, expr, mutable) =>
        val resolvedType = resolveType(tpe).now
        val typedExpr = TypeContext.inNewBlockScope(typeExpr(expr)).now

        val (finalType, castedExpr) =
          if resolvedType == tpd.Type.Inferred then
            (typedExpr.exprType, typedExpr)
          else
            (resolvedType, castOrFail(typedExpr, resolvedType).now)

        TypeContext.updateVariable(name, Variable(name, finalType, mutable, false, true)).now

        val (id, _) = TypeContext.getVariableOrFail(name).now

        tpd.Expr.Assign(id, name, castedExpr, tpd.Type.Unit)
      case untpd.Expr.Assign(name, expr) =>
        val typedExpr = typeExpr(expr).now
        val (id, variable) = TypeContext.getVariableOrFail(name).now
        if variable.mutable then
          tpd.Expr.Assign(id, name, castOrFail(typedExpr, variable.tpe).now, tpd.Type.Unit)
        else
          Typing.failAndAbort(TypeFailure.ImmutableVariableAssignment(name)).now
      case untpd.Expr.Apply(expr, args) =>
        val typedExpr = typeExpr(expr).now
        val typedArgs = args.map(typeExpr(_).now)
        def rec(tpe: tpd.Type): tpd.Expr < Typing = direct:
          tpe match
            case tpd.Type.Fun(params, output) =>
              val paramCount = params.size
              val argCount = args.size
              if paramCount == argCount then
                val castedArgs = typedArgs.zip(params).map(castOrFail(_, _).now)
                tpd.Expr.Apply(typedExpr, castedArgs, output)
              else
                Typing.failAndAbort(TypeFailure.WrongArgumentCount(argCount, paramCount)).now

            case tpd.Type.TypeFun(typeParams, funType @ tpd.Type.Fun(params, output)) =>
              val paramCount = params.size
              val argCount = args.size
              if paramCount == argCount then
                val resolvedTypes = params
                  .zip(typedArgs)
                  .collect:
                    case (tpd.Type.Generic(name), arg) if typeParams.contains(name) => (name, arg.exprType)
                  .groupMap(_._1)(_._2)
                  .map((typeParam, types) =>
                    (typeParam, Kyo.foldLeft(types)(tpd.Type.Nothing)(TypeContext.union).now)
                  )

                val replacements = resolvedTypes.toMap.withDefaultValue(tpd.Type.Nothing)
                val replacementsOrNothing = typeParams.map((_, tpd.Type.Nothing)).toMap ++ replacements

                val notInfered = typeParams.filterNot(replacements.contains)
                notInfered.foreach(typeParam => Typing.fail(TypeFailure.CannotInferType(typeParam)).now)

                if !notInfered.isEmpty then Abort.fail(()).now

                tpd.Expr.Apply(
                  typedExpr.withType(funType.replaceGeneric(replacementsOrNothing)),
                  typedArgs,
                  output.replaceGeneric(replacementsOrNothing).now
                )
              else
                Typing.failAndAbort(TypeFailure.WrongArgumentCount(argCount, paramCount)).now

            case tpd.Type.Class(name, constructor) => rec(constructor).now

            case tpe =>
              Typing.failAndAbort(TypeFailure.Mismatch(tpe, tpd.Type.Fun(typedArgs.map(_.exprType), tpd.Type.Inferred))).now

        rec(typedExpr.exprType).now
      case untpd.Expr.TypeApply(expr, types) =>
        val typedExpr = typeExpr(expr).now
        def rec(tpe: tpd.Type): tpd.Expr < Typing = direct:
          tpe match
            case tpd.Type.TypeFun(typeParams, output) =>
              val sizeCompare = types.sizeCompare(typeParams)
              if sizeCompare < 0 then
                Typing.failAndAbort(TypeFailure.MissingTypeArguments(typeParams.take(types.size))).now
              else if sizeCompare > 0 then
                Typing.failAndAbort(TypeFailure.TooManyTypeArguments(types, typeParams)).now
              else
                TypeContext.inNewBlockScope:
                  direct:
                    val replacements = typeParams
                      .zip(types)
                      .map((paramName, tpe) => (paramName, resolveType(tpe).now))
                      .toMap

                    println(s"Explicit replacements: ${pprint(replacements)}")

                    typedExpr.withType(output.replaceGeneric(replacements).now)
                .now
            case tpd.Type.Class(name, constructor) => rec(constructor).now
            case tpe => Typing.failAndAbort(TypeFailure.Mismatch(tpe, tpd.Type.TypeFun(Chunk.empty, tpd.Type.Inferred))).now

        rec(typedExpr.exprType).now
      case funDef @ untpd.Expr.FunDef(name, typeParams, params, retType, body) =>
        val id = Var.use[TypeContext](_.scopes.head.variables(name)).now

        TypeContext.inNewBlockScope:
          direct:
            val (uniqueTypeParams, resolvedParams, paramTypes, resolvedRetType, _) = declareTypeParamsAndResolveFunTypes(funDef).now

            TypeContext.inNewFunctionScope(id, name, params.map(_._1)): internalName =>
              direct:
                resolvedParams.foreach((name, tpe) => TypeContext.declareVariable(name, Variable(name, tpe, false, false, true)).now)

                val typedBody = typeExpr(body).now

                val funVariable = Var.use[TypeContext](_.variables(id.value)).now

                val (finalType, castedBody) =
                  if resolvedRetType == tpd.Type.Inferred then
                    val inferredType =
                      if typeParams.isEmpty then tpd.Type.Fun(paramTypes, typedBody.exprType)
                      else tpd.Type.TypeFun(uniqueTypeParams.map(_._2), tpd.Type.Fun(paramTypes, typedBody.exprType))
                    (inferredType, typedBody)
                  else
                    (funVariable.tpe, castOrFail(typedBody, resolvedRetType).now)

                Var.updateDiscard[TypeContext](ctx =>
                  ctx.copy(
                    variables = ctx.variables.updated(id.value, funVariable.copy(tpe = finalType, initialized = true))
                  )
                ).now

                (
                  castedBody,
                  tpd.Expr.Assign(
                    id = id,
                    name = name,
                    expr = tpd.Expr.FunRef(internalName, tpd.Type.Fun(resolvedParams.map(_._2), finalType)),
                    exprType = tpd.Type.Unit
                  )
                )
            .now
        .now

      case untpd.Expr.ClassDef(name, typeParams, parameters, body) =>
        val id = Var.use[TypeContext](_.scopes.head.variables(name)).now

        TypeContext.inNewClassScope(id, name, typeParams, parameters.map(_._1)): internalName =>
          direct:
            val (uniqueTypeParams, resolvedParams, paramTypes, resolvedRetType, constructorType) =
              declareTypeParamsAndResolveFunTypes(untpd.Expr.FunDef(
                name = Identifier.assume(s"<$name constructor>"),
                typeParams = typeParams,
                params = parameters,
                retType = untpd.Type.Ref(internalName),
                body = untpd.Expr.Block(body)
              )).now
            resolvedParams.foreach((name, tpe) => TypeContext.declareVariable(name, Variable(name, tpe, false, false, true)).now)

            val declarations = body.flatMap(typeDeclaration(_).now)
            val typedBody = body.map(typeExpr(_).now)

            (
              typedBody,
              tpd.Expr.Assign(
                id = id,
                name = name,
                expr = tpd.Expr.ClassRef(internalName, tpd.Type.Class(internalName, constructorType)),
                exprType = tpd.Type.Unit
              )
            )
        .now

      case untpd.Expr.Select(expr, name) =>
        val typedExpr = typeExpr(expr).now
        typedExpr.exprType match
          case tpd.Type.Instance(className, _, replacements) =>
            val (id, member) = TypeContext.getDeclarationOrFail(className, name).now
            tpd.Expr.Select(id, typedExpr, name, member.tpe.replaceGeneric(replacements))
          case tpe =>
            Typing.failAndAbort(TypeFailure.Mismatch(tpe, tpd.Type.Any)).now

      case untpd.Expr.Block(expressions) =>
        val declarations = expressions.flatMap(typeDeclaration(_).now)

        val typedExprs = expressions.map(typeExpr(_).now)
        val blockType = if typedExprs.isEmpty then tpd.Type.Unit else typedExprs.last.exprType
        tpd.Expr.Block(declarations, typedExprs, blockType)
      case untpd.Expr.If(cond, ifTrue, ifFalse) =>
        val typedCond = typeExpr(cond).now
        val castedCond = castOrFail(typedCond, tpd.Type.Boolean).now
        val typedIfTrue = TypeContext.inNewBlockScope(typeExpr(ifTrue)).now
        val typedIfFalse = TypeContext.inNewBlockScope(typeExpr(ifFalse)).now

        // TODO support inheritance
        if TypeContext.isSubtype(typedIfTrue.exprType, typedIfFalse.exprType).now then
          tpd.Expr.If(castedCond, typedIfTrue, typedIfFalse, typedIfFalse.exprType)
        else if TypeContext.isSubtype(typedIfFalse.exprType, typedIfTrue.exprType).now then
          tpd.Expr.If(castedCond, typedIfTrue, typedIfFalse, typedIfTrue.exprType)
        else
          Typing.failAndAbort(
            TypeFailure.Mismatch(
              typedIfTrue.exprType.zip(typedIfFalse.exprType),
              typedIfTrue.exprType.zip(typedIfTrue.exprType)
            )
          ).now
      case untpd.Expr.While(cond, body) =>
        val typedCond = typeExpr(cond).now
        val castedCond = castOrFail(typedCond, tpd.Type.Boolean).now
        val typedBody = TypeContext.inNewBlockScope(typeExpr(body)).now
        tpd.Expr.While(castedCond, typedBody, tpd.Type.Unit)
      case untpd.Expr.For(iterator, iterable, body) =>
        val typedIterable = typeExpr(iterable).now
        typedIterable.exprType match
          case iterableType @ tpd.Type.Apply(tpd.Type.Array, Chunk(elemType)) =>
            TypeContext.inNewBlockScope:
              direct:
                val iteratorId = TypeContext.declareVariable(iterator, Variable(iterator, elemType, false, false, true)).now
                val typedBody = Var.use[TypeContext](iterContext => Var.set(iterContext).flatMap(_ => typeExpr(body))).now

                val indexName = TypeContext.newUniqueVarName(Identifier("$i")).now
                val indexId = TypeContext.declareVariable(indexName, Variable(indexName, tpd.Type.Int, true, false, true)).now

                val (lengthId, _) = TypeContext.getVariableOrFail(Identifier("length")).now
                val (getId, _) = TypeContext.getVariableOrFail(Identifier("get")).now

                // TODO Once OOP is implemented, use something like an `Iterable` interface or/and a `forEach` method.
                tpd.Expr.Block(
                  declarations = Chunk((indexId, indexName)),
                  expressions = Chunk(
                    tpd.Expr.Assign(indexId, indexName, tpd.Expr.LInt(0, tpd.Type.Int), tpd.Type.Unit),
                    tpd.Expr.While(
                      cond = tpd.Expr.Less(
                        left = tpd.Expr.VarCall(indexId, indexName, tpd.Type.Int),
                        right = tpd.Expr.Apply(
                          expr =
                            tpd.Expr.VarCall(lengthId, Identifier("length"), tpd.Type.Fun(Chunk(iterableType), tpd.Type.Int)),
                          args = Chunk(typedIterable),
                          exprType = tpd.Type.Int
                        ),
                        exprType = tpd.Type.Boolean
                      ),
                      body = tpd.Expr.Block(
                        declarations = Chunk((iteratorId, iterator)),
                        expressions = Chunk(
                          tpd.Expr.Assign(
                            id = iteratorId,
                            name = iterator,
                            expr = tpd.Expr.Apply(
                              tpd.Expr.VarCall(getId, Identifier("get"), tpd.Type.Fun(Chunk(iterableType, tpd.Type.Int), elemType)),
                              Chunk(
                                typedIterable,
                                tpd.Expr.VarCall(indexId, indexName, tpd.Type.Int)
                              ),
                              tpd.Type.Generic(Identifier("A"))
                            ),
                            exprType = tpd.Type.Unit
                          ),
                          typedBody,
                          tpd.Expr.Assign(
                            id = indexId,
                            name = indexName,
                            tpd.Expr.Add(
                              tpd.Expr.VarCall(indexId, indexName, tpd.Type.Int),
                              tpd.Expr.LInt(1, tpd.Type.Int),
                              tpd.Type.Int
                            ),
                            tpd.Type.Unit
                          )
                        ),
                        exprType = tpd.Type.Unit
                      ),
                      exprType = tpd.Type.Unit
                    )
                  ),
                  exprType = tpd.Type.Unit
                )
            .now
          case tpe =>
            Typing.failAndAbort(TypeFailure.Mismatch(tpe, tpd.Type.Array)).now
