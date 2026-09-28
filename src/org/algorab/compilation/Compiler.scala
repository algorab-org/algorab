package org.algorab.compilation

import io.github.iltotore.iron.autoRefine
import org.algorab.AlgorabProgram
import org.algorab.ast.InstructionPosition
import org.algorab.ast.ParamCount
import org.algorab.ast.SymbolId
import org.algorab.ast.Value
import org.algorab.ast.compiled.Function
import org.algorab.ast.compiled.Instruction
import org.algorab.ast.compiled.Module
import org.algorab.ast.compiled.Program as CompiledProgram
import org.algorab.ast.typed
import org.algorab.ast.typed.*
import purelogic.State

/**
 * The compilation phase.
 */
object Compiler:

  /**
   * Compile an unary operator.
   *
   * @param expr the operand to compile
   * @param op the operation's instruction
   */
  def compileUnaryOp(expr: Expr, op: Instruction): Compilation[Unit] =
    compileExpr(expr)
    CompilationContext.emit(op)

  /**
   * Compile a binary operator.
   *
   * @param left the LHS to compile
   * @param right the RHS to compile
   * @param op the operation's instruction
   */
  def compileBinaryOp(left: Expr, right: Expr, op: Instruction): Compilation[Unit] =
    compileExpr(left)
    compileExpr(right)
    CompilationContext.emit(op)

  /**
   * Compile a numeric unary operator.
   *
   * @param expr the operand to compile
   * @param opInt the operation's instruction if the operand is an integer
   * @param opFloat the operation's instruction if the operand is a float
   */
  def compileUnaryNumOp(expr: Expr, opInt: Instruction, opFloat: Instruction): Compilation[Unit] =
    compileUnaryOp(
      expr = expr,
      op =
        if expr.tpe == Type.Int then opInt
        else if expr.tpe == Type.Float then opFloat
        else throw AssertionError(s"Wrong type (${expr.tpe}) for operator $opInt/$opFloat. Bug in typer?")
    )

  /**
   * Compile a numeric binary operator.
   *
   * @param left the LHS to compile
   * @param right the RHS to compile
   * @param opInt the operation's instruction if both operands are integers
   * @param opFloat the operation's instruction if both operands are floats
   */
  def compileBinaryNumOp(left: Expr, right: Expr, opInt: Instruction, opFloat: Instruction): Compilation[Unit] =
    compileBinaryOp(
      left = left,
      right = right,
      op =
        if left.tpe == Type.Int && right.tpe == Type.Int then opInt
        else if left.tpe == Type.Float && right.tpe == Type.Float then opFloat
        else throw AssertionError(s"Wrong types (${left.tpe} and ${right.tpe}) for operator $opInt/$opFloat. Bug in typer?")
    )

  /**
   * Declare global variables in the programs forming a module.
   *
   * @param moduleSymbol the symbol of the module
   * @param programs the source codes owned by this module
   */
  def declarePrograms(moduleSymbol: SymbolId, programs: Seq[Program]): Compilation[Unit] =
    programs.flatMap(_.moduleStatements).foreach:
      case definition: Definition => CompilationContext.declareGlobal(definition.symbol, moduleSymbol)
      case _                      =>

  /**
   * Compile a set of programs into a module.
   *
   * @param moduleSymbol the symbol identifying the module
   * @param programs the programs to compile
   * @return the compiled module
   */
  def compilePrograms(moduleSymbol: SymbolId, programs: Seq[Program]): Compilation[Module] =
    val initialization = Compilation.locally:
      val allStatements = programs.flatMap(_.moduleStatements)
      compileAllDeclarations(allStatements)
      allStatements.foreach(compileStatement)

    CompilationContext.addFunction(moduleSymbol, Function(initialization.toArray))

    Module(
      initialization = moduleSymbol
    )

  /**
   * Compile a statement.
   *
   * @param statement the statement to compile
   */
  def compileStatement(statement: Statement): Compilation[Unit] = statement match
    case definition: Definition => compileDefinition(definition)
    case expr: Expr             => compileExpr(expr)

  /**
   * Compile the definition of all declarations in a sequence of statements.
   *
   * @param statements the statements containing the declarations
   */
  def compileAllDeclarations(statements: Seq[Statement]): Compilation[Unit] =
    statements.foreach:
      case definition: Definition => compileDeclaration(definition)
      case _                      =>

  /**
   * Compile a definition initialization.
   *
   * @param definition the definition to declare
   */
  def compileDeclaration(definition: Definition): Compilation[Unit] = definition match
    case Definition.Val(symbol, tpe, _, _, position) =>
      CompilationContext.emit(Instruction.Push(Value.default(tpe), position))
      CompilationContext.emitStore(symbol, position)

    case Definition.Function(symbol, _, _, _, position) =>
      CompilationContext.emit(Instruction.Push(Value(null), position))
      CompilationContext.emitStore(symbol, position)

  /**
   * Compile a definition.
   *
   * @param definition the definition to compile
   */
  def compileDefinition(definition: Definition): Compilation[Unit] = definition match
    case Definition.Val(symbol, _, expr, _, position) =>
      compileExpr(expr)
      CompilationContext.emitStore(symbol, position)

    case Definition.Function(symbol, params, retType, body, position) =>
      val instructions = Compilation.locally:
        params.reverse.foreach((param, _) => CompilationContext.emit(Instruction.Store(param, position)))
        compileExpr(body)
        CompilationContext.emit(Instruction.Return(position))

      CompilationContext.addFunction(symbol, Function(instructions.toArray))
      CompilationContext.emit(Instruction.Push(Value.FunctionRef(symbol), position))
      CompilationContext.emitStore(symbol, position)

  /**
   * Compile an expression.
   *
   * @param expr the expression to compile
   */
  def compileExpr(expr: Expr): Compilation[Unit] = expr match
    case Expr.LBool(value, _, position)          => CompilationContext.emit(Instruction.Push(Value(value), position))
    case Expr.LInt(value, _, position)           => CompilationContext.emit(Instruction.Push(Value(value), position))
    case Expr.LFloat(value, _, position)         => CompilationContext.emit(Instruction.Push(Value(value), position))
    case Expr.LChar(value, _, position)          => CompilationContext.emit(Instruction.Push(Value(value), position))
    case Expr.LString(value, _, position)        => CompilationContext.emit(Instruction.Push(Value(value), position))
    case Expr.Not(expr, _, position)             => compileUnaryOp(expr, Instruction.Not(position))
    case Expr.Equal(left, right, _, position)    => compileBinaryOp(left, right, Instruction.Equal(position))
    case Expr.NotEqual(left, right, _, position) => compileBinaryOp(left, right, Instruction.NotEqual(position))
    case Expr.Less(left, right, _, position)     => compileBinaryNumOp(left, right, Instruction.LessInt(position), Instruction.LessFloat(position))
    case Expr.LessEqual(left, right, _, position) =>
      compileBinaryNumOp(left, right, Instruction.LessEqualInt(position), Instruction.LessEqualFloat(position))
    case Expr.Greater(left, right, _, position) =>
      compileBinaryNumOp(left, right, Instruction.GreaterInt(position), Instruction.GreaterFloat(position))
    case Expr.GreaterEqual(left, right, _, position) =>
      compileBinaryNumOp(left, right, Instruction.GreaterEqualInt(position), Instruction.GreaterEqualFloat(position))
    case Expr.Plus(expr, _, position)          => compileExpr(expr)
    case Expr.Minus(expr, _, position)         => compileUnaryNumOp(expr, Instruction.MinusInt(position), Instruction.MinusFloat(position))
    case Expr.Add(left, right, _, position)    => compileBinaryNumOp(left, right, Instruction.AddInt(position), Instruction.AddFloat(position))
    case Expr.Sub(left, right, _, position)    => compileBinaryNumOp(left, right, Instruction.SubInt(position), Instruction.SubFloat(position))
    case Expr.Mul(left, right, _, position)    => compileBinaryNumOp(left, right, Instruction.MulInt(position), Instruction.MulFloat(position))
    case Expr.Div(left, right, _, position)    => compileBinaryNumOp(left, right, Instruction.DivInt(position), Instruction.DivFloat(position))
    case Expr.IntDiv(left, right, _, position) => compileBinaryNumOp(left, right, Instruction.IntDivInt(position), Instruction.IntDivFloat(position))
    case Expr.Mod(left, right, _, position)    => compileBinaryNumOp(left, right, Instruction.ModInt(position), Instruction.ModFloat(position))
    case Expr.And(left, right, _, position) =>
      compileExpr(left)
      val (rightInstructions, rightEnd) = Compilation.locallyAt(CompilationContext.currentPosition + 1):
        compileExpr(right)
        CompilationContext.emit(Instruction.Jump(CompilationContext.currentPosition + 2, position))

      CompilationContext.emit(Instruction.JumpIfFalse(rightEnd, position))
      CompilationContext.emitAll(rightInstructions)
      CompilationContext.emit(Instruction.Push(Value(false), position))

    case Expr.Or(left, right, _, position) =>
      compileExpr(left)
      val rightStart = CompilationContext.currentPosition + 3
      CompilationContext.emit(Instruction.JumpIfFalse(rightStart, position))
      val (rightInstructions, rightEnd) = Compilation.locallyAt(rightStart)(compileExpr(right))
      CompilationContext.emit(Instruction.Push(Value(true), position))
      CompilationContext.emit(Instruction.Jump(rightEnd, position))
      CompilationContext.emitAll(rightInstructions)

    case Expr.VarCall(symbol, _, position) => CompilationContext.emitLoad(symbol, position)
    case Expr.Assign(symbol, expr, _, position) =>
      compileExpr(expr)
      CompilationContext.emitStore(symbol, position)
    case Expr.ToFloat(expr, position) =>
      compileExpr(expr)
      CompilationContext.emit(Instruction.ToFloat(position))
    case Expr.Apply(expr, args, _, position) =>
      args.foreach(compileExpr)
      compileExpr(expr)
      CompilationContext.emit(Instruction.Apply(ParamCount.assume(args.size), position))
    case Expr.Block(statements, _, position) =>
      compileAllDeclarations(statements)
      statements.foreach(compileStatement)
    case Expr.If(cond, ifTrue, ifFalse, _, position) =>
      compileExpr(cond)
      val ifTrueStart = CompilationContext.currentPosition + 1
      val (ifTrueInstructions, ifTrueEnd) = Compilation.locallyAt(ifTrueStart)(compileExpr(ifTrue))
      val ifFalseStart = ifTrueEnd + 1
      val (ifFalseInstructions, ifFalseEnd) = Compilation.locallyAt(ifFalseStart)(compileExpr(ifFalse))

      CompilationContext.emit(Instruction.JumpIfFalse(ifFalseStart, position))
      CompilationContext.emitAll(ifTrueInstructions)
      CompilationContext.emit(Instruction.Jump(ifFalseEnd, position))
      CompilationContext.emitAll(ifFalseInstructions)
    case Expr.While(cond, body, _, position) =>
      val whileStart = CompilationContext.currentPosition
      compileExpr(cond)
      val bodyStart = CompilationContext.currentPosition + 1
      val (bodyInstructions, bodyEnd) = Compilation.locallyAt(bodyStart):
        compileExpr(body)
        CompilationContext.emit(Instruction.Jump(whileStart, position))

      CompilationContext.emit(Instruction.JumpIfFalse(bodyEnd, position))
      CompilationContext.emitAll(bodyInstructions)

    case Expr.For(iterator, iterable, body, _, position) => ???
    case Expr.Invalid(_, position)                       => throw AssertionError(s"Tried to compile an invalid node at $position")

  /**
   * Compile programs into one.
   *
   * @param programs the programs to compile
   * @return the compiled program
   */
  def apply(programs: Seq[Program]): AlgorabProgram[CompiledProgram] =
    val (context, modules) = Compilation(
      programs
        .groupBy(_.symbol)
        .tapEach(declarePrograms)
        .map((id, programs) => (id, compilePrograms(id, programs)))
    )

    CompiledProgram(
      modules = modules,
      functions = context.functions,
      owners = context.globals
    )
