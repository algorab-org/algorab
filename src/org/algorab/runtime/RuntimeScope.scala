package org.algorab.runtime

import kyo.Console
import org.algorab.ast.Identifier
import org.algorab.compiler.Value

case class RuntimeScope(variables: Map[Identifier, Value]):

  def getVariable(name: Identifier): Option[Value] = variables.get(name)

object RuntimeScope:

  val empty: RuntimeScope = RuntimeScope(Map.empty)

  val builtins: RuntimeScope = RuntimeScope(Map(
    Identifier("println") -> Value.BuiltInFunction(args =>
      Console.printLine(args.head).andThen(Value.VUnit)
    )
  ))