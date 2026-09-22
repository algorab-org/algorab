package org.algorab.runtime

import io.github.iltotore.pureparser.Span
import org.algorab.AlgorabProgram
import org.algorab.ast.SymbolId
import org.algorab.ast.Value
import org.algorab.ast.compiled.Instruction
import org.algorab.ast.compiled.Module
import org.algorab.ast.compiled.Program
import org.algorab.ast.typed.Type
import org.algorab.runtime.RuntimeContext.currentFrame
import purelogic.*

/**
 * The runtime phase, based on a stack VM.
 */
object VM:

  /**
   * Pop a boolean value from the stack.
   *
   * @param span the source span of the operation
   * @return the popped value
   */
  def popBool(span: Span): Runtime[Boolean] = RuntimeContext.pop match
    case value: Boolean => value
    case value          => fail(RuntimeError.simpleMismatch(Type.Boolean, value, span))

  /**
   * Pop an integer value from the stack.
   *
   * @param span the source span of the operation
   * @return the popped value
   */
  def popInt(span: Span): Runtime[Int] = RuntimeContext.pop match
    case value: Int => value
    case value      => fail(RuntimeError.simpleMismatch(Type.Int, value, span))

  /**
   * Pop a floating-point value from the stack.
   *
   * @param span the source span of the operation
   * @return the popped value
   */
  def popFloat(span: Span): Runtime[Double] = RuntimeContext.pop match
    case value: Double => value
    case value         => fail(RuntimeError.simpleMismatch(Type.Float, value, span))

  /**
   * Apply a binary operation to two values.
   *
   * @param right the right operand
   * @param left the left operand
   * @param op the operation to apply
   * @return the resulting value
   */
  inline def binaryOp[A <: Value.Raw](right: A, left: A, inline op: (A, A) => Value.Raw): Value =
    Value(op(left, right))

  /**
   * Apply a binary operation to two integer values popped from the stack.
   *
   * @param span the source span of the operation
   * @param op the operation to apply
   * @return the resulting value
   */
  inline def binaryOpInt(inline span: Span, inline op: (Int, Int) => Value.Raw): Runtime[Value] =
    binaryOp(popInt(span), popInt(span), op)

  /**
   * Apply a binary operation to two floating-point values popped from the stack.
   *
   * @param span the source span of the operation
   * @param op the operation to apply
   * @return the resulting value
   */
  inline def binaryOpFloat(inline span: Span, inline op: (Double, Double) => Value.Raw): Runtime[Value] =
    binaryOp(popFloat(span), popFloat(span), op)

  /**
   * Load a module and its dependencies.
   *
   * @param module the module to load
   */
  def loadModule(id: SymbolId, module: Module): Runtime[Unit] =
    update(ctx => ctx.copy(initializedModules = ctx.initializedModules + id))
    RuntimeContext.pushNewFrame(module.initialization, List.empty)

  /**
   * Interpret an instruction.
   *
   * @param instruction the instruction to interpret
   */
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
    case Instruction.LoadGlobal(symbol, span) =>
      val ctx = get
      val moduleId = ctx.owners(symbol)

      if ctx.initializedModules.contains(moduleId) then RuntimeContext.push(RuntimeContext.loadGlobal(symbol))
      else
        loadModule(moduleId, ctx.modules(moduleId))

    case Instruction.Apply(paramCount, span) =>
      val function = RuntimeContext.pop

      function match
        case Value.FunctionRef(symbol) => RuntimeContext.pushNewFrame(
            symbol,
            RuntimeContext.popN(paramCount.value)
          )

        case Value.BuiltinFunction(f) =>
          RuntimeContext.push(f(RuntimeContext.popN(paramCount.value)))

        case _ =>
          fail(RuntimeError.simpleMismatch(Type.Function(List.fill(paramCount.value)(Type.Any), Type.Any), function, span))

    case Instruction.Jump(to, span)        => RuntimeContext.jump(to)
    case Instruction.JumpIfFalse(to, span) => if !popBool(span) then RuntimeContext.jump(to)
    case Instruction.Return(span) =>
      val returned = RuntimeContext.pop
      RuntimeContext.popFrame()
      RuntimeContext.push(returned)

  /**
   * Execute a compiled program.
   *
   * @param program the program to execute
   */
  def apply(program: Program): AlgorabProgram[Unit] =
    Runtime(program):
      loadModule(SymbolId.Root, program.modules(SymbolId.Root))
      while RuntimeContext.isRunning do
        val instruction = RuntimeContext.nextInstruction
        interpret(instruction)
