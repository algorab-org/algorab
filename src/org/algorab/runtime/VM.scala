package org.algorab.runtime

import kyo.*
import org.algorab.compiler.Instruction
import org.algorab.compiler.Value
import org.algorab.ast.Identifier

object VM:

  def matchOrError[A, B](x: A)(f: PartialFunction[A, B]): B =
    f.applyOrElse(x, a => throw AssertionError(s"Unexpected matching value: $a"))

  def interpretInstr(instruction: Instruction): Unit < Runtime = direct:
    instruction match
      case Instruction.Push(value)   => RuntimeContext.push(value).now
      case Instruction.Declare(name) => RuntimeContext.declareVariable(name, RuntimeContext.pop.now).now
      case Instruction.Assign(name) =>
        RuntimeContext.assignVariable(name, RuntimeContext.pop.now).now
      case Instruction.Load(name) => RuntimeContext.push(RuntimeContext.getVariable(name).now).now
      case Instruction.LoadFunction(name) =>
        val function = RuntimeContext.getFunction(name).now
        val capturedVars = function.captures.foldLeft(Map.empty[Identifier, Value])((map, varName) =>
          map.updated(varName, RuntimeContext.getVariable(varName).now)  
        )
        RuntimeContext.push(
          Value.UserDefinedFunction(
            function.start,
            capturedVars
          )
        ).now
      case Instruction.Not        => RuntimeContext.push(Value.VBool(!RuntimeContext.pop.now.asBool)).now
      case Instruction.Equal => RuntimeContext.push(Value.VBool(
          RuntimeContext.pop.now == RuntimeContext.pop.now
        )).now
      case Instruction.NotEqual => RuntimeContext.push(Value.VBool(
          RuntimeContext.pop.now != RuntimeContext.pop.now
        )).now
      case Instruction.Less => matchOrError((RuntimeContext.pop.now, RuntimeContext.pop.now)) {
          case (Value.VInt(a), Value.VInt(b))     => RuntimeContext.push(Value.VBool(a < b))
          case (Value.VFloat(a), Value.VFloat(b)) => RuntimeContext.push(Value.VBool(a < b))
        }.now
      case Instruction.LessEqual => matchOrError((RuntimeContext.pop.now, RuntimeContext.pop.now)) {
          case (Value.VInt(a), Value.VInt(b))     => RuntimeContext.push(Value.VBool(a <= b))
          case (Value.VFloat(a), Value.VFloat(b)) => RuntimeContext.push(Value.VBool(a <= b))
        }.now
      case Instruction.Greater => matchOrError((RuntimeContext.pop.now, RuntimeContext.pop.now)) {
          case (Value.VInt(a), Value.VInt(b))     => RuntimeContext.push(Value.VBool(a > b))
          case (Value.VFloat(a), Value.VFloat(b)) => RuntimeContext.push(Value.VBool(a > b))
        }.now
      case Instruction.GreaterEqual => matchOrError((RuntimeContext.pop.now, RuntimeContext.pop.now)) {
          case (Value.VInt(a), Value.VInt(b))     => RuntimeContext.push(Value.VBool(a >= b))
          case (Value.VFloat(a), Value.VFloat(b)) => RuntimeContext.push(Value.VBool(a >= b))
        }.now
      case Instruction.Minus => matchOrError(RuntimeContext.pop.now) {
          case Value.VInt(value)   => RuntimeContext.push(Value.VInt(value))
          case Value.VFloat(value) => RuntimeContext.push(Value.VFloat(value))
        }.now
      case Instruction.Add => matchOrError((RuntimeContext.pop.now, RuntimeContext.pop.now)) {
          case (Value.VInt(a), Value.VInt(b))     => RuntimeContext.push(Value.VInt(a + b))
          case (Value.VFloat(a), Value.VFloat(b)) => RuntimeContext.push(Value.VFloat(a + b))
          // case (Value.VString(b), Value.VString(b)) => RuntimeContext.push(Value.VString(a + b))
        }.now
      case Instruction.Sub => matchOrError((RuntimeContext.pop.now, RuntimeContext.pop.now)) {
          case (Value.VInt(a), Value.VInt(b))     => RuntimeContext.push(Value.VInt(a - b))
          case (Value.VFloat(a), Value.VFloat(b)) => RuntimeContext.push(Value.VFloat(a - b))
        }.now
      case Instruction.Mul => matchOrError((RuntimeContext.pop.now, RuntimeContext.pop.now)) {
          case (Value.VInt(a), Value.VInt(b))     => RuntimeContext.push(Value.VInt(a * b))
          case (Value.VFloat(a), Value.VFloat(b)) => RuntimeContext.push(Value.VFloat(a * b))
        }.now
      case Instruction.Div => matchOrError((RuntimeContext.pop.now, RuntimeContext.pop.now)) {
          case (Value.VInt(a), Value.VInt(b))     => RuntimeContext.push(Value.VFloat(a.toDouble / b))
          case (Value.VFloat(a), Value.VFloat(b)) => RuntimeContext.push(Value.VFloat(a / b))
        }.now
      case Instruction.IntDiv => matchOrError((RuntimeContext.pop.now, RuntimeContext.pop.now)) {
          case (Value.VInt(a), Value.VInt(b))     => RuntimeContext.push(Value.VInt(a / b))
          case (Value.VFloat(a), Value.VFloat(b)) => RuntimeContext.push(Value.VInt((a / b).toInt))
        }.now
      case Instruction.Mod => matchOrError((RuntimeContext.pop.now, RuntimeContext.pop.now)) {
          case (Value.VInt(a), Value.VInt(b))     => RuntimeContext.push(Value.VInt(a % b))
          case (Value.VFloat(a), Value.VFloat(b)) => RuntimeContext.push(Value.VFloat(a % b))
        }.now
      case Instruction.And => matchOrError((RuntimeContext.pop.now, RuntimeContext.pop.now)) {
          case (Value.VBool(a), Value.VBool(b)) => RuntimeContext.push(Value.VBool(a && b))
        }.now
      case Instruction.Or => matchOrError((RuntimeContext.pop.now, RuntimeContext.pop.now)) {
          case (Value.VBool(a), Value.VBool(b)) => RuntimeContext.push(Value.VBool(a || b))
        }.now
      case Instruction.Apply(paramCount) =>
        val function = RuntimeContext.pop.now
        val args = Chunk.range(0, paramCount.value).map(_ => RuntimeContext.pop.now)

        matchOrError(function) {
          case Value.UserDefinedFunction(start, capturedVars) =>
            direct:
              val frame = RuntimeFrame(
                start,
                args,
                Chunk(RuntimeScope(capturedVars))
              )

              RuntimeContext.pushFrame(frame).now
          case Value.BuiltInFunction(f) => f(args).map(RuntimeContext.push)
        }.now
      case Instruction.Jump(position) => RuntimeContext.jump(position).now
      case Instruction.JumpIf(ifTrue, ifFalse) =>
        if RuntimeContext.pop.now.asBool then
          RuntimeContext.jump(ifTrue).now
        else
          RuntimeContext.jump(ifFalse).now
      case Instruction.Return =>
        val returnValue = RuntimeContext.pop.now
        RuntimeContext.popFrame.now
        RuntimeContext.push(returnValue).now
      case Instruction.PushScope => RuntimeContext.pushScope.now
      case Instruction.PopScope  => RuntimeContext.popScope.now
      case Instruction.FunctionStart(internalName, displayName, captures, next) => 
        val functionDef = FunctionDef(
          displayName,
          captures,
          RuntimeContext.nextInstruction.now
        )

        RuntimeContext.declareFunction(internalName, functionDef).now
        RuntimeContext.jump(next).now

  def interpretAll(instructions: Chunk[Instruction]): Unit < Runtime = direct:
    while instructions.sizeCompare(RuntimeContext.nextInstruction.now.value) > 0 do
      val next = RuntimeContext.nextInstruction.now
      RuntimeContext.jump(RuntimeContext.nextInstruction.now + 1).now
      interpretInstr(instructions(next.value)).now
