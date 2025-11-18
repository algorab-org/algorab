package org.algorab.runtime

import kyo.*
import org.algorab.ast.Identifier
import org.algorab.compiler.Value
import org.algorab.compiler.InstrPosition

case class RuntimeContext(nextInstruction: InstrPosition, stack: Chunk[Value], scopes: Chunk[RuntimeScope]):

  def jump(nextInstruction: InstrPosition): RuntimeContext =
    this.copy(nextInstruction = nextInstruction)

  def push(value: Value): RuntimeContext =
    this.copy(stack = value +: stack)

  def pop: (Value, RuntimeContext) =
    (stack.head, this.copy(stack = stack.tail))

  def pushScope: RuntimeContext =
    this.copy(scopes = RuntimeScope.empty +: scopes)

  def popScope: RuntimeContext =
    this.copy(scopes = scopes.tail)

  def getVariable(name: Identifier): Option[Value] =
    scopes.collectFirst(((scope: RuntimeScope) => scope.getVariable(name)).unlift)

  def declareVariable(name: Identifier, value: Value): RuntimeContext =
    this.copy(scopes = scopes.headOption match
      case Some(head) => RuntimeScope(head.variables + (name -> value)) +: scopes.tail
      case None       => RuntimeScope(Map(name -> value)) +: scopes
    )

  def assignVariable(name: Identifier, value: Value): RuntimeContext =
    def rec(scopes: Chunk[RuntimeScope]): Chunk[RuntimeScope] = scopes match
      case head +: tail =>
        if head.variables.contains(name) then
          head.copy(variables = head.variables.updated(name, value)) +: tail
        else
          head +: rec(tail)
      
      case _ => throw AssertionError(s"Unknown variable: $name")
      
    this.copy(scopes = rec(scopes))

object RuntimeContext:

  val empty: RuntimeContext = RuntimeContext(
    nextInstruction = InstrPosition(0),
    stack = Chunk.empty,
    scopes = Chunk(RuntimeScope.builtins)
  )

  def modify(f: RuntimeContext => RuntimeContext < Runtime): Unit < Runtime =
    Var.use[RuntimeContext](f)
      .map(Var.set)
      .unit

  def modifyReturn[A](f: RuntimeContext => (A, RuntimeContext) < Runtime): A < Runtime =
    Var.use[RuntimeContext](f)
      .map((result, newState) => Var.set(newState).andThen(result))

  def nextInstruction: InstrPosition < Runtime = Var.use[RuntimeContext](_.nextInstruction)

  def jump(nextInstruction: InstrPosition): Unit < Runtime = modify(_.jump(nextInstruction))

  def push(value: Value): Unit < Runtime = modify(_.push(value))
  
  def pop: Value < Runtime = modifyReturn(_.pop)

  def pushScope: Unit < Runtime = modify(_.pushScope)

  def popScope: Unit < Runtime = modify(_.popScope)

  def getVariable(name: Identifier): Value < Runtime =
    Var.use[RuntimeContext](
      _
        .getVariable(name)
        .getOrElse(throw AssertionError(s"Unknown variable: $name"))
    )

  def declareVariable(name: Identifier, value: Value): Unit < Runtime =
    modify(_.declareVariable(name, value))

  def assignVariable(name: Identifier, value: Value): Unit < Runtime =
    modify(_.assignVariable(name, value))
    