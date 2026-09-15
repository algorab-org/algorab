package org.algorab.runtime

import io.github.iltotore.pureparser.Span
import org.algorab.ast.Value
import org.algorab.ast.compiled.Instruction
import org.algorab.ast.compiled.Module
import org.algorab.ast.typed.Type
import purelogic.*
import org.algorab.ast.compiled.Program
import org.algorab.ast.SymbolId
import org.algorab.AlgorabProgram
import org.algorab.runtime.RuntimeContext.currentFrame

object VM:

  def popBool(span: Span): Runtime[Boolean] = RuntimeContext.pop match
    case value: Boolean => value
    case value          => fail(RuntimeError.simpleMismatch(Type.Boolean, value, span))

  def popInt(span: Span): Runtime[Int] = RuntimeContext.pop match
    case value: Int => value
    case value      => fail(RuntimeError.simpleMismatch(Type.Int, value, span))

  def popFloat(span: Span): Runtime[Double] = RuntimeContext.pop match
    case value: Double => value
    case value         => fail(RuntimeError.simpleMismatch(Type.Float, value, span))

  inline def binaryOp[A <: Value.Raw](inline right: A, inline left: A, inline op: (A, A) => Value.Raw): Value =
    Value(op(left, right))

  inline def binaryOpInt(inline span: Span, inline op: (Int, Int) => Value.Raw): Runtime[Value] =
    binaryOp(popInt(span), popInt(span), op)

  inline def binaryOpFloat(inline span: Span, inline op: (Double, Double) => Value.Raw): Runtime[Value] =
    binaryOp(popFloat(span), popFloat(span), op)

  def loadModule(module: Module): Runtime[Unit] =
    for dependency <- module.dependencies do
      loadModule(RuntimeContext.getModule(dependency))

    RuntimeContext.pushNewFrame(module.initialization, List.empty)

  def interpret(instruction: Instruction): Runtime[Unit] = instruction match
    case Instruction.Push(value, span)         => RuntimeContext.push(value)
    case Instruction.Not(span)                 => RuntimeContext.push(Value(!popBool(span)))
    case Instruction.Equal(span)               => RuntimeContext.push(Value(RuntimeContext.pop == RuntimeContext.pop))
    case Instruction.NotEqual(span)            => RuntimeContext.push(Value(RuntimeContext.pop != RuntimeContext.pop))
    case Instruction.ToFloat(span)             => RuntimeContext.push(Value(popInt(span).toFloat))
    case Instruction.LessInt(span)             => RuntimeContext.push(binaryOpInt(span, _ < _))
    case Instruction.LessEqualInt(span)        => RuntimeContext.push(binaryOpInt(span, _ <= _))
    case Instruction.GreaterInt(span)          => RuntimeContext.push(binaryOpInt(span, _ > _))
    case Instruction.GreaterEqualInt(span)     => RuntimeContext.push(binaryOpInt(span, _ >= _))
    case Instruction.MinusInt(span)            => RuntimeContext.push(Value(-popInt(span)))
    case Instruction.AddInt(span)              => RuntimeContext.push(binaryOpInt(span, _ + _))
    case Instruction.SubInt(span)              => RuntimeContext.push(binaryOpInt(span, _ - _))
    case Instruction.MulInt(span)              => RuntimeContext.push(binaryOpInt(span, _ * _))
    case Instruction.DivInt(span)              => RuntimeContext.push(binaryOpInt(span, (left, right) => left.toDouble / right))
    case Instruction.IntDivInt(span)           => RuntimeContext.push(binaryOpInt(span, _ / _))
    case Instruction.ModInt(span)              => RuntimeContext.push(binaryOpInt(span, _ % _))
    case Instruction.LessFloat(span)           => RuntimeContext.push(binaryOpFloat(span, _ < _))
    case Instruction.LessEqualFloat(span)      => RuntimeContext.push(binaryOpFloat(span, _ <= _))
    case Instruction.GreaterFloat(span)        => RuntimeContext.push(binaryOpFloat(span, _ > _))
    case Instruction.GreaterEqualFloat(span)   => RuntimeContext.push(binaryOpFloat(span, _ >= _))
    case Instruction.MinusFloat(span)          => RuntimeContext.push(Value(-popFloat(span)))
    case Instruction.AddFloat(span)            => RuntimeContext.push(binaryOpFloat(span, _ + _))
    case Instruction.SubFloat(span)            => RuntimeContext.push(binaryOpFloat(span, _ - _))
    case Instruction.MulFloat(span)            => RuntimeContext.push(binaryOpFloat(span, _ * _))
    case Instruction.DivFloat(span)            => RuntimeContext.push(binaryOpFloat(span, _ / _))
    case Instruction.IntDivFloat(span)         => RuntimeContext.push(binaryOpFloat(span, (left, right) => left.toInt / right.toInt))
    case Instruction.ModFloat(span)            => RuntimeContext.push(binaryOpFloat(span, _ % _))
    case Instruction.Store(symbol, span)       => RuntimeContext.storeLocal(symbol, RuntimeContext.pop)
    case Instruction.StoreGlobal(symbol, span) => RuntimeContext.storeGlobal(symbol, RuntimeContext.pop)
    case Instruction.Load(symbol, span)        => RuntimeContext.push(RuntimeContext.loadLocal(symbol))
    case Instruction.LoadGlobal(symbol, span)  => RuntimeContext.push(RuntimeContext.loadGlobal(symbol))
    case Instruction.Apply(paramCount, span)   =>
      val function = RuntimeContext.pop
      
      function match
        case Value.FunctionRef(symbol) => RuntimeContext.pushNewFrame(
          symbol,
          RuntimeContext.currentFrame.stack.take(paramCount.value)
        )

        case Value.BuiltinFunction(f) => RuntimeContext.push(f(RuntimeContext.currentFrame.stack.take(paramCount.value)))

        case _ =>
          fail(RuntimeError.simpleMismatch(Type.Function(List.fill(paramCount.value)(Type.Any), Type.Any), function, span))
      
    case Instruction.Jump(to, span)            => RuntimeContext.jump(to)
    case Instruction.JumpIfFalse(to, span)     => if !popBool(span) then RuntimeContext.jump(to)
    case Instruction.Return(span)              =>
      val returned = RuntimeContext.pop
      RuntimeContext.popFrame()
      RuntimeContext.push(returned)

  def apply(program: Program): AlgorabProgram[Unit] =
    Runtime(program):
      loadModule(program.modules(SymbolId.Root))
      while RuntimeContext.isRunning do
        val instruction = RuntimeContext.nextInstruction
        interpret(instruction) 
