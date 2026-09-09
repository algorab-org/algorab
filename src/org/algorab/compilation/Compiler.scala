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

object Compiler:

  def compileUnaryOp(expr: Expr, op: Instruction): Compilation[Unit] =
    compileExpr(expr)
    CompilationContext.emit(op)

  def compileBinaryOp(left: Expr, right: Expr, op: Instruction): Compilation[Unit] =
    compileExpr(left)
    compileExpr(right)
    CompilationContext.emit(op)

  def compilePrograms(programs: Seq[Program]): Compilation[Module] = Module(
    dependencies = Set.empty,
    initialization = Compilation.locally:
      val allStatements = programs.flatMap(_.moduleStatements)
      compileAllDeclarations(allStatements)
      allStatements.foreach(compileStatement)
  )

  def compileStatement(statement: Statement): Compilation[Unit] = statement match
    case definition: Definition => compileDefinition(definition)
    case expr: Expr             => compileExpr(expr)

  def compileAllDeclarations(statements: Seq[Statement]): Compilation[Unit] =
    statements.foreach:
      case definition: Definition => compileDeclaration(definition)
      case _ =>

  def compileDeclaration(definition: Definition): Compilation[Unit] = definition match
    case Definition.Val(symbol, tpe, _, _, span) =>
      CompilationContext.emit(Instruction.Push(Value.default(tpe), span))
      CompilationContext.emit(Instruction.Store(symbol, span))

    case Definition.Function(symbol, params, retType, body, span) =>
      val instructions = Compilation.locally:
        params.reverse.foreach((param, _) => CompilationContext.emit(Instruction.Store(param, span)))
        compileExpr(body)
        CompilationContext.emit(Instruction.Return(span))
      
      CompilationContext.addFunction(symbol, Function(instructions))
      CompilationContext.emit(Instruction.Push(Value.FunctionRef(symbol), span))
      CompilationContext.emit(Instruction.Store(symbol, span))

  def compileDefinition(definition: Definition): Compilation[Unit] = definition match
    case Definition.Val(symbol, _, expr, _, span) =>
      compileExpr(expr)
      CompilationContext.emit(Instruction.Store(symbol, span))

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
    case Expr.Less(left, right, _, span)         => compileBinaryOp(left, right, Instruction.Less(span))
    case Expr.LessEqual(left, right, _, span)    => compileBinaryOp(left, right, Instruction.LessEqual(span))
    case Expr.Greater(left, right, _, span)      => compileBinaryOp(left, right, Instruction.Greater(span))
    case Expr.GreaterEqual(left, right, _, span) => compileBinaryOp(left, right, Instruction.GreaterEqual(span))
    case Expr.Plus(expr, _, span)                => compileExpr(expr)
    case Expr.Minus(expr, _, span)               => compileUnaryOp(expr, Instruction.Minus(span))
    case Expr.Add(left, right, _, span)          => compileBinaryOp(left, right, Instruction.Add(span))
    case Expr.Sub(left, right, _, span)          => compileBinaryOp(left, right, Instruction.Sub(span))
    case Expr.Mul(left, right, _, span)          => compileBinaryOp(left, right, Instruction.Mul(span))
    case Expr.Div(left, right, _, span)          => compileBinaryOp(left, right, Instruction.Div(span))
    case Expr.IntDiv(left, right, _, span)       => compileBinaryOp(left, right, Instruction.IntDiv(span))
    case Expr.Mod(left, right, _, span)          => compileBinaryOp(left, right, Instruction.Mod(span))
    case Expr.And(left, right, _, span)          => compileBinaryOp(left, right, Instruction.And(span))
    case Expr.Or(left, right, _, span)           => compileBinaryOp(left, right, Instruction.Or(span))
    case Expr.VarCall(symbol, _, span)           => CompilationContext.emit(Instruction.Load(symbol, span))
    case Expr.Assign(symbol, expr, _, span)      => compileUnaryOp(expr, Instruction.Store(symbol, span))
    case Expr.Apply(expr, args, _, span) =>
      compileExpr(expr)
      args.foreach(compileExpr)
      CompilationContext.emit(Instruction.Apply(ParamCount.assume(args.size), span))
    case Expr.Block(statements, _, span) =>
      compileAllDeclarations(statements)
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
    val (context, modules) = Compilation(programs.groupBy(_.moduleSymbol).map((id, programs) => (id, compilePrograms(programs))))
    
    CompiledProgram(
      modules = modules,
      functions = context.functions
    )