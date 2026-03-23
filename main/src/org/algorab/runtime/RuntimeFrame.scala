package org.algorab.runtime

import kyo.Chunk
import org.algorab.ast.Identifier
import org.algorab.compiler.InstrPosition
import org.algorab.compiler.Value

case class RuntimeFrame(
    nextInstruction: InstrPosition,
    stack: Chunk[Value],
    scopes: Chunk[RuntimeScope]
):

  def jump(nextInstruction: InstrPosition): RuntimeFrame =
    this.copy(nextInstruction = nextInstruction)

  def push(value: Value): RuntimeFrame =
    this.copy(stack = value +: stack)

  def pop: (Value, RuntimeFrame) =
    (stack.head, this.copy(stack = stack.tail))

  def pushScope: RuntimeFrame =
    this.copy(scopes = RuntimeScope.empty +: scopes)

  def popScope: RuntimeFrame =
    this.copy(scopes = scopes.tail)

  def getVariable(name: Identifier): Option[Value] =
    scopes.collectFirst(((scope: RuntimeScope) => scope.getVariable(name)).unlift)

  def declareVariable(name: Identifier): RuntimeFrame =
    this.copy(scopes = scopes.headOption match
      case Some(head) => RuntimeScope(head.variables + (name -> null)) +: scopes.tail
      case None       => RuntimeScope(Map(name -> null)) +: scopes
    )

  def declareBox(name: Identifier): RuntimeFrame =
    this.copy(scopes = scopes.headOption match
      case Some(head) => RuntimeScope(head.variables + (name -> Value.VBox(null))) +: scopes.tail
      case None       => RuntimeScope(Map(name -> Value.VBox(null))) +: scopes
    )

  def assignVariable(name: Identifier, value: Value): RuntimeFrame =
    def rec(scopes: Chunk[RuntimeScope]): Chunk[RuntimeScope] = scopes match
      case head +: tail =>
        if head.variables.contains(name) then
          head.copy(variables = head.variables.updated(name, value)) +: tail
        else
          head +: rec(tail)

      case _ => throw AssertionError(s"Unknown variable: $name")

    this.copy(scopes = rec(scopes))

object RuntimeFrame:

  val root: RuntimeFrame = RuntimeFrame(
    nextInstruction = InstrPosition(0),
    stack = Chunk.empty,
    scopes = Chunk(RuntimeScope.builtins)
  )
