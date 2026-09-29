package org.algorab.runtime

import org.algorab.AlgorabProgram
import org.algorab.ast.SymbolId
import org.algorab.ast.Value
import org.algorab.ast.compiled.Instruction
import org.algorab.ast.compiled.Module
import org.algorab.ast.compiled.Program
import org.algorab.ast.typed.Type
import org.algorab.runtime.RuntimeContext.currentFrame
import org.algorab.util.SourcePosition
import purelogic.*

/**
 * The runtime phase, based on a stack VM.
 */
object VM:

  /**
   * Pop a boolean value from the stack.
   *
   * @param position the source position of the operation
   * @return the popped value
   */
  def popBool(position: SourcePosition): Runtime[Boolean] = RuntimeContext.pop match
    case value: Boolean => value
    case value          => fail(RuntimeError.simpleMismatch(Type.Boolean, value, position))

  /**
   * Pop an integer value from the stack.
   *
   * @param position the source position of the operation
   * @return the popped value
   */
  def popInt(position: SourcePosition): Runtime[Int] = RuntimeContext.pop match
    case value: Int => value
    case value      => fail(RuntimeError.simpleMismatch(Type.Int, value, position))

  /**
   * Pop a floating-point value from the stack.
   *
   * @param position the source position of the operation
   * @return the popped value
   */
  def popFloat(position: SourcePosition): Runtime[Double] = RuntimeContext.pop match
    case value: Double => value
    case value         => fail(RuntimeError.simpleMismatch(Type.Float, value, position))

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
   * @param position the source position of the operation
   * @param op the operation to apply
   * @return the resulting value
   */
  inline def binaryOpInt(inline position: SourcePosition, inline op: (Int, Int) => Value.Raw): Runtime[Value] =
    binaryOp(popInt(position), popInt(position), op)

  /**
   * Apply a binary operation to two floating-point values popped from the stack.
   *
   * @param position the source position of the operation
   * @param op the operation to apply
   * @return the resulting value
   */
  inline def binaryOpFloat(inline position: SourcePosition, inline op: (Double, Double) => Value.Raw): Runtime[Value] =
    binaryOp(popFloat(position), popFloat(position), op)

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
    case Instruction.Push(value, position)         => RuntimeContext.push(value)
    case Instruction.Not(position)                 => RuntimeContext.push(Value(!popBool(position)))
    case Instruction.Equal(position)               => RuntimeContext.push(Value(RuntimeContext.pop == RuntimeContext.pop))
    case Instruction.NotEqual(position)            => RuntimeContext.push(Value(RuntimeContext.pop != RuntimeContext.pop))
    case Instruction.ToFloat(position)             => RuntimeContext.push(Value(popInt(position).toFloat))
    case Instruction.LessInt(position)             => RuntimeContext.push(binaryOpInt(position, _ < _))
    case Instruction.LessEqualInt(position)        => RuntimeContext.push(binaryOpInt(position, _ <= _))
    case Instruction.GreaterInt(position)          => RuntimeContext.push(binaryOpInt(position, _ > _))
    case Instruction.GreaterEqualInt(position)     => RuntimeContext.push(binaryOpInt(position, _ >= _))
    case Instruction.MinusInt(position)            => RuntimeContext.push(Value(-popInt(position)))
    case Instruction.AddInt(position)              => RuntimeContext.push(binaryOpInt(position, _ + _))
    case Instruction.SubInt(position)              => RuntimeContext.push(binaryOpInt(position, _ - _))
    case Instruction.MulInt(position)              => RuntimeContext.push(binaryOpInt(position, _ * _))
    case Instruction.DivInt(position)              => RuntimeContext.push(binaryOpInt(position, (left, right) => left.toDouble / right))
    case Instruction.IntDivInt(position)           => RuntimeContext.push(binaryOpInt(position, _ / _))
    case Instruction.ModInt(position)              => RuntimeContext.push(binaryOpInt(position, _ % _))
    case Instruction.LessFloat(position)           => RuntimeContext.push(binaryOpFloat(position, _ < _))
    case Instruction.LessEqualFloat(position)      => RuntimeContext.push(binaryOpFloat(position, _ <= _))
    case Instruction.GreaterFloat(position)        => RuntimeContext.push(binaryOpFloat(position, _ > _))
    case Instruction.GreaterEqualFloat(position)   => RuntimeContext.push(binaryOpFloat(position, _ >= _))
    case Instruction.MinusFloat(position)          => RuntimeContext.push(Value(-popFloat(position)))
    case Instruction.AddFloat(position)            => RuntimeContext.push(binaryOpFloat(position, _ + _))
    case Instruction.SubFloat(position)            => RuntimeContext.push(binaryOpFloat(position, _ - _))
    case Instruction.MulFloat(position)            => RuntimeContext.push(binaryOpFloat(position, _ * _))
    case Instruction.DivFloat(position)            => RuntimeContext.push(binaryOpFloat(position, _ / _))
    case Instruction.IntDivFloat(position)         => RuntimeContext.push(binaryOpFloat(position, (left, right) => left.toInt / right.toInt))
    case Instruction.ModFloat(position)            => RuntimeContext.push(binaryOpFloat(position, _ % _))
    case Instruction.Store(symbol, position)       => RuntimeContext.storeLocal(symbol, RuntimeContext.pop)
    case Instruction.StoreGlobal(symbol, position) => RuntimeContext.storeGlobal(symbol, RuntimeContext.pop)
    case Instruction.Load(symbol, position)        => RuntimeContext.push(RuntimeContext.loadLocal(symbol))
    case Instruction.LoadGlobal(symbol, position) =>
      val ctx = get
      val moduleId = ctx.owners(symbol)

      if ctx.initializedModules.contains(moduleId) then RuntimeContext.push(RuntimeContext.loadGlobal(symbol))
      else
        loadModule(moduleId, ctx.modules(moduleId))

    case Instruction.Apply(paramCount, position) =>
      val function = RuntimeContext.pop

      function match
        case Value.FunctionRef(symbol) => RuntimeContext.pushNewFrame(
            symbol,
            RuntimeContext.popN(paramCount.value)
          )

        case Value.BuiltinFunction(f) =>
          RuntimeContext.push(f(RuntimeContext.popN(paramCount.value)))

        case _ =>
          fail(RuntimeError.simpleMismatch(Type.Function(List.fill(paramCount.value)(Type.Any), Type.Any), function, position))

    case Instruction.Jump(to, position)        => RuntimeContext.jump(to)
    case Instruction.JumpIfFalse(to, position) => if !popBool(position) then RuntimeContext.jump(to)
    case Instruction.Return(position) =>
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
