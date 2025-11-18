package org.algorab.compiler

import kyo.*
import org.algorab.ast.tpd.Expr

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
      case Expr.VarCall(name, _)       => Compilation.emit(Instruction.Load(name)).now
      case Expr.ValDef(name, _, expr, _, _) =>
        compileExpr(expr).now
        Compilation.emit(Instruction.Declare(name)).now
      case Expr.Assign(name, expr, _) =>
        compileExpr(expr).now
        Compilation.emit(Instruction.Assign(name)).now
      case Expr.Apply(expr, args, _) =>
        Kyo.foreachDiscard(args.reverse)(compileExpr).now
        compileExpr(expr).now
        Compilation.emit(Instruction.Apply(ParamCount.assume(args.size))).now

      case Expr.FunDef(name, params, retType, body, _) => ??? // TODO
      case Expr.Block(expressions, _)                  =>
        Compilation.emit(Instruction.PushScope).now
        Kyo.foreachDiscard(expressions)(compileExpr).now
        Compilation.emit(Instruction.PopScope).now
      case Expr.If(cond, ifTrue, ifFalse, _) =>
        compileExpr(cond).now

        val ifTrueInstrs = Compilation.run(compileExpr(ifTrue)).now
        val ifFalseInstrs = Compilation.run(compileExpr(ifFalse)).now

        val ifTrueStart = Compilation.nextPosition.now + 1
        val ifFalseStart = ifTrueStart + ifTrueInstrs.size + 1
        val ifNext = ifFalseStart + ifFalseInstrs.size

        Compilation.emit(Instruction.JumpIf(ifTrueStart, ifFalseStart)).now
        Compilation.emitAll(ifTrueInstrs).now
        Compilation.emit(Instruction.Jump(ifNext)).now
        Compilation.emitAll(ifFalseInstrs).now

      case Expr.While(cond, body, _) =>
        val condStart = Compilation.nextPosition.now

        compileExpr(cond).now

        val bodyStart = Compilation.nextPosition.now + 1

        val bodyInstrs = Compilation.run(compileExpr(body)).now

        val loopEnd = bodyStart + bodyInstrs.size + 1

        Compilation.emit(Instruction.JumpIf(bodyStart, loopEnd)).now
        Compilation.emitAll(bodyInstrs).now
        Compilation.emit(Instruction.Jump(condStart)).now
