/**
 * Code generator: lowers the typed AST to a flat sequence of [[Instruction]]s.
 *
 * The [[Compiler]] object contains a single public method, [[compileProgram]], which drives
 * the full compilation of a program.  Internally it delegates to [[compileExpr]] for
 * expressions, [[compileFunction]] for function definitions, and [[compileClass]] for
 * class definitions.
 *
 * The target is a simple stack machine.  Expressions push their results; operators pop
 * operands and push results.  Control-flow instructions ([[Instruction.Jump]],
 * [[Instruction.JumpIf]]) use absolute [[InstrPosition]] addresses that are
 * pre-computed by running sub-compilations in isolation via [[Compilation.run]].
 *
 * === Short-circuit evaluation ===
 *
 * `And` and `Or` are compiled with conditional jumps so that the right operand is only
 * evaluated when necessary:
 *
 *   - `left and right` →
 *     {{{
 *       <left>
 *       JumpIf(rightStart, andEnd)
 *       <right>
 *       Jump(andEnd)
 *       Push(false)
 *     }}}
 *   - `left or right` →
 *     {{{
 *       <left>
 *       JumpIf(orEnd, rightStart)
 *       <right>
 *       Jump(orEnd)
 *       Push(true)
 *     }}}
 */
package org.algorab.compiler

import kyo.*
import org.algorab.ast.Identifier
import org.algorab.ast.tpd.Expr
import org.algorab.ast.tpd.Type
import org.algorab.typer.ClassTypeDef
import org.algorab.typer.FunctionDef
import org.algorab.typer.TypeContext

/** Compiles typed Algorab AST nodes to bytecode instructions. */
object Compiler:

  /** The special identifier used for the implicit `this` parameter in class constructors. */
  private val thisIdentifier = Identifier("this")

  /**
   * Returns the epilogue instructions appended after every compiled function body.
   *
   * For `Unit`-returning functions a `Push(VUnit)` is inserted before `Return` so the
   * caller always finds a value on the stack.  Non-`Unit` functions rely on the body
   * expression having already left a value on the stack.
   *
   * @param function the function definition whose return type is inspected
   * @return one or two trailing instructions
   */
  private def functionEpilogue(function: FunctionDef): Chunk[Instruction] =
    if function.body.exprType == Type.Unit then
      Chunk(Instruction.Push(Value.VUnit), Instruction.Return)
    else
      Chunk(Instruction.Return)

  /**
   * Epilogue appended after every compiled class constructor body.
   *
   * Loads `this` and returns it so the caller receives the newly constructed instance.
   */
  private val classEpilogue: Chunk[Instruction] = Chunk(Instruction.loadThis, Instruction.Return)

  /**
   * Compiles a binary operator expression by evaluating both operands and then emitting
   * the operator instruction.
   *
   * Stack effect: `( -- left right result )`
   *
   * @param left        the left-hand operand expression
   * @param right       the right-hand operand expression
   * @param instruction the binary operator instruction to emit after the operands
   */
  def compileBinaryOp(left: Expr, right: Expr, instruction: Instruction): Unit < Compilation = direct:
    compileExpr(left).now
    compileExpr(right).now
    Compilation.emit(instruction).now

  /**
   * Compiles a single typed expression, emitting the corresponding instructions.
   *
   * Each case maps to one or more instructions that together implement the expression's
   * semantics on the stack machine.  See the [[Instruction]] documentation for the
   * stack effect of each instruction.
   *
   * Noteworthy cases:
   *   - `And` / `Or` – short-circuit evaluated via conditional jumps (see object-level docs).
   *   - `ValDef` – emits a declaration followed by the initialiser and an assignment.
   *   - `Block` – emits [[Instruction.PushScope]] / [[Instruction.PopScope]] around the body;
   *     forward declarations are emitted before the body expressions.
   *   - `If` – pre-compiles both branches to compute their sizes, then emits a `JumpIf`.
   *   - `While` – emits the condition, then the body behind a conditional jump, with an
   *     unconditional jump back to the condition at the end.
   *
   * @param expr the typed expression to compile
   */
  def compileExpr(expr: Expr): Unit < Compilation = direct:
    expr match
      case Expr.LBool(value, _)   => Compilation.emit(Instruction.Push(Value.VBool(value))).now
      case Expr.LInt(value, _)    => Compilation.emit(Instruction.Push(Value.VInt(value))).now
      case Expr.LFloat(value, _)  => Compilation.emit(Instruction.Push(Value.VFloat(value))).now
      case Expr.LChar(value, _)   => Compilation.emit(Instruction.Push(Value.VChar(value))).now
      case Expr.LString(value, _) => Compilation.emit(Instruction.Push(Value.VString(value))).now
      case Expr.Not(expr, _) =>
        compileExpr(expr).now
        Compilation.emit(Instruction.Not).now
      case Expr.Equal(left, right, _)        => compileBinaryOp(left, right, Instruction.Equal).now
      case Expr.NotEqual(left, right, _)     => compileBinaryOp(left, right, Instruction.NotEqual).now
      case Expr.Less(left, right, _)         => compileBinaryOp(left, right, Instruction.Less).now
      case Expr.LessEqual(left, right, _)    => compileBinaryOp(left, right, Instruction.LessEqual).now
      case Expr.Greater(left, right, _)      => compileBinaryOp(left, right, Instruction.Greater).now
      case Expr.GreaterEqual(left, right, _) => compileBinaryOp(left, right, Instruction.GreaterEqual).now
      case Expr.Minus(expr, _) =>
        compileExpr(expr).now
        Compilation.emit(Instruction.Minus).now
      case Expr.Add(left, right, _)    => compileBinaryOp(left, right, Instruction.Add).now
      case Expr.Sub(left, right, _)    => compileBinaryOp(left, right, Instruction.Sub).now
      case Expr.Mul(left, right, _)    => compileBinaryOp(left, right, Instruction.Mul).now
      case Expr.Div(left, right, _)    => compileBinaryOp(left, right, Instruction.Div).now
      case Expr.IntDiv(left, right, _) => compileBinaryOp(left, right, Instruction.IntDiv).now
      case Expr.Mod(left, right, _)    => compileBinaryOp(left, right, Instruction.Mod).now
      case Expr.And(left, right, _) =>
        compileExpr(left).now

        val rightStart = Compilation.nextPosition.now + 1
        val compiledRight = Compilation.run(rightStart)(compileExpr(right)).now

        val rightEnd = rightStart + compiledRight.size + 1
        val andEnd = rightEnd + 1

        Compilation.emit(Instruction.JumpIf(rightStart, rightEnd)).now
        Compilation.emitAll(compiledRight).now
        Compilation.emit(Instruction.Jump(andEnd)).now
        Compilation.emit(Instruction.Push(Value.VBool(false))).now

      case Expr.Or(left, right, _) =>
        compileExpr(left).now

        val rightStart = Compilation.nextPosition.now + 1
        val compiledRight = Compilation.run(rightStart)(compileExpr(right)).now

        val rightEnd = rightStart + compiledRight.size + 1
        val orEnd = rightEnd + 1

        Compilation.emit(Instruction.JumpIf(rightEnd, rightStart)).now
        Compilation.emitAll(compiledRight).now
        Compilation.emit(Instruction.Jump(orEnd)).now
        Compilation.emit(Instruction.Push(Value.VBool(true))).now

      case Expr.VarCall(id, name, _) =>
        val variable = Compilation.getVariable(id).now
        Compilation.emitAll(Instruction.load(name, variable.boxxed, variable.field)).now
      case Expr.ValDef(id, name, _, expr, _) =>
        val variable = Compilation.getVariable(id).now
        Compilation.emitAll(Instruction.declare(name, variable.boxxed, variable.field)).now
        compileExpr(expr).now
        Compilation.emitAll(Instruction.assign(name, variable.boxxed, variable.field)).now

      case Expr.Assign(id, name, expr, _) =>
        val variable = Compilation.getVariable(id).now
        compileExpr(expr).now
        Compilation.emitAll(Instruction.assign(name, variable.boxxed, variable.field)).now
      case Expr.Apply(expr, args, _) =>
        Kyo.foreachDiscard(args)(compileExpr).now
        compileExpr(expr).now
        Compilation.emit(Instruction.Apply(ParamCount.assume(args.size))).now

      case Expr.FunRef(name, _)   => Compilation.emit(Instruction.LoadFunction(name)).now
      case Expr.ClassRef(name, _) => Compilation.emit(Instruction.LoadClass(name)).now
      case Expr.Select(id, expr, name, _) =>
        compileExpr(expr).now
        Compilation.emit(Instruction.Select(name)).now

      case Expr.Block(declarations, expressions, _) =>
        Compilation.emit(Instruction.PushScope).now
        declarations.foreach((id, name) =>
          val variable = Compilation.getVariable(id).now
          Compilation.emitAll(Instruction.declare(name, variable.boxxed, variable.field)).now
        ).now
        Kyo.foreachDiscard(expressions)(compileExpr).now
        Compilation.emit(Instruction.PopScope).now
      case Expr.If(cond, ifTrue, ifFalse, _) =>
        compileExpr(cond).now

        val ifTrueStart = Compilation.nextPosition.now + 1
        val ifTrueInstrs = Compilation.run(ifTrueStart)(compileExpr(ifTrue)).now

        val ifFalseStart = ifTrueStart + ifTrueInstrs.size + 1
        val ifFalseInstrs = Compilation.run(ifFalseStart)(compileExpr(ifFalse)).now

        val ifNext = ifFalseStart + ifFalseInstrs.size

        Compilation.emit(Instruction.JumpIf(ifTrueStart, ifFalseStart)).now
        Compilation.emitAll(ifTrueInstrs).now
        Compilation.emit(Instruction.Jump(ifNext)).now
        Compilation.emitAll(ifFalseInstrs).now

      case Expr.While(cond, body, _) =>
        val condStart = Compilation.nextPosition.now

        compileExpr(cond).now

        val bodyStart = Compilation.nextPosition.now + 1

        val bodyInstrs = Compilation.run(bodyStart)(compileExpr(body)).now

        val loopEnd = bodyStart + bodyInstrs.size + 1

        Compilation.emit(Instruction.JumpIf(bodyStart, loopEnd)).now
        Compilation.emitAll(bodyInstrs).now
        Compilation.emit(Instruction.Jump(condStart)).now

  /**
   * Compiles a single function definition into the instruction stream.
   *
   * Layout of the emitted instructions:
   * {{{
   *   FunctionStart(internalName, displayName, captures, <end>)
   *   Declare(param0) ; Assign(param0)   -- one pair per parameter
   *   ...
   *   <body instructions>
   *   [Push(VUnit)] ; Return             -- epilogue
   * }}}
   *
   * The `FunctionStart` instruction records the capture list and jumps past the
   * function body when encountered at the top level (so the body is not executed
   * during top-level initialisation).
   *
   * @param internalName the unique internal name for the function (typer-assigned)
   * @param function     the function definition (parameters, captures, body)
   */
  def compileFunction(internalName: Identifier, function: FunctionDef): Unit < Compilation = direct:
    val argsInstrs = function.params.flatMap(arg =>
      Chunk(
        Instruction.Declare(arg),
        Instruction.Assign(arg)
      )
    )
    val bodyPos = Compilation.nextPosition.now + argsInstrs.size + 1
    val bodyInstrs = Compilation.run(bodyPos)(compileExpr(function.body)).now
    val epilogueInstrs = functionEpilogue(function)

    val localCaptures = function.captures.map(id => Compilation.getVariable(id).now.localName)

    Compilation.emit(Instruction.FunctionStart(internalName, function.displayName, localCaptures, bodyPos + bodyInstrs.size + epilogueInstrs.size)).now
    Compilation.emitAll(argsInstrs).now
    Compilation.emitAll(bodyInstrs).now
    Compilation.emitAll(epilogueInstrs).now

  /**
   * Compiles a single class definition into the instruction stream.
   *
   * Layout of the emitted constructor instructions:
   * {{{
   *   ClassStart(internalName, displayName, <end>)
   *   loadThis ; DeclareField(field0)   -- one pair per declared field (except "this")
   *   ...
   *   loadThis ; AssignField(param0)    -- one pair per constructor parameter
   *   ...
   *   <body init instructions>
   *   loadThis ; Return                 -- classEpilogue
   * }}}
   *
   * The `ClassStart` instruction records the constructor entry point and jumps past
   * the constructor body during top-level initialisation.
   *
   * @param internalName the unique internal name for the class (typer-assigned)
   * @param clazz        the class type definition (declarations, parameters, init body)
   */
  def compileClass(internalName: Identifier, clazz: ClassTypeDef): Unit < Compilation = direct:
    val initPos = Compilation.nextPosition.now + 1
    val defInstrs = Compilation.run(initPos)(Kyo.foreachDiscard(clazz.declarations)(tpl =>
      if tpl._1 == thisIdentifier then Kyo.unit
      else Compilation.emitAll(Chunk(Instruction.loadThis, Instruction.DeclareField(tpl._1)))
    )).now

    val paramInstrs = Compilation.run(initPos + defInstrs.size)(Kyo.foreachDiscard(clazz.parameters)(name =>
      Compilation.emitAll(Chunk(Instruction.loadThis, Instruction.AssignField(name)))
    )).now

    val initInstrs = Compilation.run(initPos + defInstrs.size + paramInstrs.size)(Kyo.foreachDiscard(clazz.init)(compileExpr)).now
    Compilation.emit(Instruction.ClassStart(
      internalName,
      clazz.displayName,
      initPos + defInstrs.size + paramInstrs.size + initInstrs.size + classEpilogue.size
    )).now

    Compilation.emitAll(defInstrs).now
    Compilation.emitAll(paramInstrs).now
    Compilation.emitAll(initInstrs).now
    Compilation.emitAll(classEpilogue).now

  /**
   * Compiles an entire Algorab program.
   *
   * Emits all function definitions first (so they are registered at the start of
   * execution), then all class definitions, and finally the main expression.
   *
   * @param main the top-level program expression (the "main body")
   */
  def compileProgram(main: Expr): Unit < Compilation = direct:
    Compilation.functions.now.foreach(compileFunction(_, _).now)
    Compilation.classes.now.foreach(compileClass(_, _).now)
    compileExpr(main).now
