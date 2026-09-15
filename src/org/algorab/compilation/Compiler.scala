package org.algorab.compilation

import io.github.iltotore.iron.autoRefine
import org.algorab.ast.compiled.Function
import org.algorab.ast.compiled.Instruction
import org.algorab.ast.compiled.Module
import org.algorab.ast.compiled.Program as CompiledProgram
import org.algorab.ast.InstructionPosition
import org.algorab.ast.ParamCount
import org.algorab.ast.Value
import org.algorab.ast.typed.*
import org.algorab.ast.typed
import purelogic.State
import org.algorab.AlgorabProgram
import org.algorab.ast.SymbolId

object Compiler:

  def compileUnaryOp(expr: Expr, op: Instruction): Compilation[Unit] =
    compileExpr(expr)
    CompilationContext.emit(op)

  def compileBinaryOp(left: Expr, right: Expr, op: Instruction): Compilation[Unit] =
    compileExpr(left)
    compileExpr(right)
    CompilationContext.emit(op)

  def compileUnaryNumOp(expr: Expr, opInt: Instruction, opFloat: Instruction): Compilation[Unit] =
    compileUnaryOp(
      expr = expr,
      op =
        if expr.tpe == Type.Int then opInt
        else if expr.tpe == Type.Float then opFloat
        else throw AssertionError(s"Wrong type (${expr.tpe}) for operator $opInt/$opFloat. Bug in typer?")
    )

  def compileBinaryNumOp(left: Expr, right: Expr, opInt: Instruction, opFloat: Instruction): Compilation[Unit] =
    compileBinaryOp(
      left = left,
      right = right,
      op =
        if left.tpe == Type.Int && right.tpe == Type.Int then opInt
        else if left.tpe == Type.Float && right.tpe == Type.Float then opFloat
        else throw AssertionError(s"Wrong types (${left.tpe} and ${right.tpe}) for operator $opInt/$opFloat. Bug in typer?")
    )

  def compilePrograms(moduleSymbol: SymbolId, programs: Seq[Program]): Compilation[Module] =
    val initialization = Compilation.locally:
      val allStatements = programs.flatMap(_.moduleStatements)
      compileAllDeclarations(allStatements, true)
      allStatements.foreach(compileStatement)

    CompilationContext.addFunction(moduleSymbol, Function(initialization.toArray))
    
    Module(
      dependencies = Set.empty,
      initialization = moduleSymbol
    )

  def compileStatement(statement: Statement): Compilation[Unit] = statement match
    case definition: Definition => compileDefinition(definition)
    case expr: Expr             => compileExpr(expr)

  def compileAllDeclarations(statements: Seq[Statement], global: Boolean): Compilation[Unit] =
    statements.foreach:
      case definition: Definition =>
        CompilationContext.declareGlobal(definition.symbol)
        compileDeclaration(definition)
      case _ =>

  def compileDeclaration(definition: Definition): Compilation[Unit] = definition match
    case Definition.Val(symbol, tpe, _, _, span) =>
      CompilationContext.emit(Instruction.Push(Value.default(tpe), span))
      CompilationContext.emitStore(symbol, span)

    case Definition.Function(symbol, params, retType, body, span) =>
      val instructions = Compilation.locally:
        params.reverse.foreach((param, _) => CompilationContext.emit(Instruction.Store(param, span)))
        compileExpr(body)
        CompilationContext.emit(Instruction.Return(span))
      
      CompilationContext.addFunction(symbol, Function(instructions.toArray))
      CompilationContext.emit(Instruction.Push(Value.FunctionRef(symbol), span))
      CompilationContext.emitStore(symbol, span)

  def compileDefinition(definition: Definition): Compilation[Unit] = definition match
    case Definition.Val(symbol, _, expr, _, span) =>
      compileExpr(expr)
      CompilationContext.emitStore(symbol, span)

    case Definition.Function(_, _, _, _, _) =>

  def compileExpr(expr: Expr): Compilation[Unit] = expr match
    case Expr.LBool(value, _, span)              => CompilationContext.emit(Instruction.Push(Value(value), span))
    case Expr.LInt(value, _, span)               => CompilationContext.emit(Instruction.Push(Value(value), span))
    case Expr.LFloat(value, _, span)             => CompilationContext.emit(Instruction.Push(Value(value), span))
    case Expr.LChar(value, _, span)              => CompilationContext.emit(Instruction.Push(Value(value), span))
    case Expr.LString(value, _, span)            => CompilationContext.emit(Instruction.Push(Value(value), span))
    case Expr.Not(expr, _, span)                 => compileUnaryOp(expr, Instruction.Not(span))
    case Expr.Equal(left, right, _, span)        => compileBinaryOp(left, right, Instruction.Equal(span))
    case Expr.NotEqual(left, right, _, span)     => compileBinaryOp(left, right, Instruction.NotEqual(span))
    case Expr.Less(left, right, _, span)         => compileBinaryNumOp(left, right, Instruction.LessInt(span), Instruction.LessFloat(span))
    case Expr.LessEqual(left, right, _, span)    => compileBinaryNumOp(left, right, Instruction.LessEqualInt(span), Instruction.LessEqualFloat(span))
    case Expr.Greater(left, right, _, span)      => compileBinaryNumOp(left, right, Instruction.GreaterInt(span), Instruction.GreaterFloat(span))
    case Expr.GreaterEqual(left, right, _, span) => compileBinaryNumOp(left, right, Instruction.GreaterEqualInt(span), Instruction.GreaterEqualFloat(span))
    case Expr.Plus(expr, _, span)                => compileExpr(expr)
    case Expr.Minus(expr, _, span)               => compileUnaryNumOp(expr, Instruction.MinusInt(span), Instruction.MinusFloat(span))
    case Expr.Add(left, right, _, span)          => compileBinaryNumOp(left, right, Instruction.AddInt(span), Instruction.AddFloat(span))
    case Expr.Sub(left, right, _, span)          => compileBinaryNumOp(left, right, Instruction.SubInt(span), Instruction.SubFloat(span))
    case Expr.Mul(left, right, _, span)          => compileBinaryNumOp(left, right, Instruction.MulInt(span), Instruction.MulFloat(span))
    case Expr.Div(left, right, _, span)          => compileBinaryNumOp(left, right, Instruction.DivInt(span), Instruction.DivFloat(span))
    case Expr.IntDiv(left, right, _, span)       => compileBinaryNumOp(left, right, Instruction.IntDivInt(span), Instruction.IntDivFloat(span))
    case Expr.Mod(left, right, _, span)          => compileBinaryNumOp(left, right, Instruction.ModInt(span), Instruction.ModFloat(span))
    case Expr.And(left, right, _, span)          =>
      compileExpr(left)
      val (rightInstructions, rightEnd) = Compilation.locallyAt(CompilationContext.currentPosition):
        compileExpr(right)
        CompilationContext.emit(Instruction.Jump(CompilationContext.currentPosition + 1, span))
      
      CompilationContext.emit(Instruction.JumpIfFalse(rightEnd, span))
      CompilationContext.emitAll(rightInstructions)
      CompilationContext.emit(Instruction.Push(Value(false), span))

    case Expr.Or(left, right, _, span)           =>
      compileExpr(left)
      val rightStart = CompilationContext.currentPosition + 3
      CompilationContext.emit(Instruction.JumpIfFalse(rightStart, span))
      val (rightInstructions, rightEnd) = Compilation.locallyAt(rightStart)(compileExpr(right))
      CompilationContext.emit(Instruction.Push(Value(true), span))
      CompilationContext.emit(Instruction.Jump(rightEnd, span))
      CompilationContext.emitAll(rightInstructions)

    case Expr.VarCall(symbol, _, span)           => CompilationContext.emitLoad(symbol, span)
    case Expr.Assign(symbol, expr, _, span)      =>
      compileExpr(expr)
      CompilationContext.emitStore(symbol, span)
    case Expr.ToFloat(expr, span) =>
      compileExpr(expr)
      CompilationContext.emit(Instruction.ToFloat(span))
    case Expr.Apply(expr, args, _, span) =>
      args.foreach(compileExpr)
      compileExpr(expr)
      CompilationContext.emit(Instruction.Apply(ParamCount.assume(args.size), span))
    case Expr.Block(statements, _, span) =>
      compileAllDeclarations(statements, false)
      statements.foreach(compileStatement)
    case Expr.If(cond, ifTrue, ifFalse, _, span) =>
      compileExpr(cond)
      val ifTrueStart = CompilationContext.currentPosition + 1
      val (ifTrueInstructions, ifTrueEnd) = Compilation.locallyAt(ifTrueStart)(compileExpr(ifTrue))
      val ifFalseStart = ifTrueEnd + 1
      val (ifFalseInstructions, ifFalseEnd) = Compilation.locallyAt(ifFalseStart)(compileExpr(ifTrue))

      CompilationContext.emit(Instruction.JumpIfFalse(ifFalseStart, span))
      CompilationContext.emitAll(ifTrueInstructions)
      CompilationContext.emit(Instruction.Jump(ifFalseEnd, span))
      CompilationContext.emitAll(ifFalseInstructions)
    case Expr.While(cond, body, _, span) =>
      compileExpr(cond)
      val bodyStart = CompilationContext.currentPosition + 1
      val (bodyInstructions, bodyEnd) = Compilation.locallyAt(bodyStart):
        compileExpr(body)
        CompilationContext.emit(Instruction.Jump(CompilationContext.currentPosition, span))

      CompilationContext.emit(Instruction.JumpIfFalse(bodyEnd, span))
      CompilationContext.emitAll(bodyInstructions)

    case Expr.For(iterator, iterable, body, _, span) => ???
    case Expr.Invalid(_, span)                       => throw AssertionError(s"Tried to compile an invalid node at $span")

  def apply(programs: Seq[Program]): AlgorabProgram[CompiledProgram] =
    val (context, modules) = Compilation(
      programs
        .groupBy(_.moduleSymbol)
        .map((id, programs) => (id, compilePrograms(id, programs)))
    )
    
    CompiledProgram(
      modules = modules,
      functions = context.functions
    )