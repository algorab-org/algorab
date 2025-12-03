package org.algorab.compiler

import kyo.*
import org.algorab.ast.tpd.Expr
import org.algorab.ast.Identifier
import org.algorab.typer.FunctionDef
import org.algorab.typer.TypeContext

object Compiler:

  def compileBinaryOp(left: Expr, right: Expr, instruction: Instruction): Unit < Compilation = direct:
    compileExpr(right).now
    compileExpr(left).now
    Compilation.emit(instruction).now

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
      case Expr.And(left, right, _)    => compileBinaryOp(left, right, Instruction.And).now
      case Expr.Or(left, right, _)     => compileBinaryOp(left, right, Instruction.Or).now
      case Expr.VarCall(id, name, _)       =>
        val boxxed = Compilation.isBoxxed(id).now
        Compilation.emit(Instruction.load(name, boxxed)).now
      case Expr.ValDef(id, name, _, expr, _) =>
        val boxxed = Compilation.isBoxxed(id).now
        Compilation.emit(Instruction.declare(name, boxxed)).now
        compileExpr(expr).now
        Compilation.emit(Instruction.assign(name, boxxed)).now
        
      case Expr.Assign(id, name, expr, _) =>
        val boxxed = Compilation.isBoxxed(id).now
        compileExpr(expr).now
        Compilation.emit(Instruction.assign(name, boxxed)).now
      case Expr.Apply(expr, args, _) =>
        Kyo.foreachDiscard(args.reverse)(compileExpr).now
        compileExpr(expr).now
        Compilation.emit(Instruction.Apply(ParamCount.assume(args.size))).now

      case Expr.FunRef(name, _) => Compilation.emit(Instruction.LoadFunction(name)).now
      case Expr.Block(declarations, expressions, _)                  =>
        Compilation.emit(Instruction.PushScope).now
        declarations.foreach((id, name) =>
          val boxxed = Compilation.isBoxxed(id).now
          Compilation.emit(Instruction.declare(name, boxxed)).now
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


  def compileFunction(internalName: Identifier, function: FunctionDef): Unit < Compilation = direct:
    val argsInstrs = function.params.flatMap(arg =>
      Chunk(
        Instruction.Declare(arg),
        Instruction.Assign(arg)
      )
    )
    val bodyPos = Compilation.nextPosition.now + argsInstrs.size + 1
    val bodyInstrs = Compilation.run(bodyPos)(compileExpr(function.body)).now

    Compilation.emit(Instruction.FunctionStart(internalName, function.displayName, function.captures, bodyPos + bodyInstrs.size + 1)).now
    Compilation.emitAll(argsInstrs).now
    Compilation.emitAll(bodyInstrs).now
    Compilation.emit(Instruction.Return).now

  def compileProgram(main: Expr): Unit < Compilation = direct:
    Compilation.functions.now.foreach(compileFunction(_, _).now)
    compileExpr(main).now