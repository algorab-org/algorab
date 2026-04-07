package org.algorab.compiler

import kyo.*
import org.algorab.ast.Identifier
import org.algorab.ast.tpd.Expr
import org.algorab.typer.ClassTypeDef
import org.algorab.typer.FunctionDef
import org.algorab.typer.TypeContext

object Compiler:

  def compileBinaryOp(left: Expr, right: Expr, instruction: Instruction): Unit < Compilation = direct:
    compileExpr(left).now
    compileExpr(right).now
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

  def compileFunction(internalName: Identifier, function: FunctionDef): Unit < Compilation = direct:
    val argsInstrs = function.params.flatMap(arg =>
      Chunk(
        Instruction.Declare(arg),
        Instruction.Assign(arg)
      )
    )
    val bodyPos = Compilation.nextPosition.now + argsInstrs.size + 1
    val bodyInstrs = Compilation.run(bodyPos)(compileExpr(function.body)).now
    val epilogueInstrCount = if function.body.exprType == org.algorab.ast.tpd.Type.Unit then 2 else 1

    val localCaptures = function.captures.map(id => Compilation.getVariable(id).now.localName)

    Compilation.emit(Instruction.FunctionStart(internalName, function.displayName, localCaptures, bodyPos + bodyInstrs.size + epilogueInstrCount)).now
    Compilation.emitAll(argsInstrs).now
    Compilation.emitAll(bodyInstrs).now
    if function.body.exprType == org.algorab.ast.tpd.Type.Unit then
      Compilation.emit(Instruction.Push(Value.VUnit)).now
    Compilation.emit(Instruction.Return).now

  def compileClass(internalName: Identifier, clazz: ClassTypeDef): Unit < Compilation = direct:
    val initPos = Compilation.nextPosition.now + 1
    val defInstrs = Compilation.run(initPos)(Kyo.foreachDiscard(clazz.declarations)(tpl =>
        if tpl._1 == Identifier("this") then Kyo.unit
        else Compilation.emitAll(Chunk(Instruction.loadThis, Instruction.DeclareField(tpl._1)))
    )).now
    val initInstrs = Compilation.run(initPos + defInstrs.size)(Kyo.foreachDiscard(clazz.init)(compileExpr)).now
    Compilation.emit(Instruction.ClassStart(internalName, clazz.displayName, initPos + defInstrs.size + initInstrs.size + 2)).now
    
    Compilation.emitAll(defInstrs).now
    Compilation.emitAll(initInstrs).now
    Compilation.emit(Instruction.loadThis).now
    Compilation.emit(Instruction.Return).now

  def compileProgram(main: Expr): Unit < Compilation = direct:
    println(pprint(main))
    Compilation.functions.now.foreach(compileFunction(_, _).now)
    Compilation.classes.now.foreach(compileClass(_, _).now)
    compileExpr(main).now
