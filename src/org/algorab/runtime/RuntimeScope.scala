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
      Console.printLine(args.head.convertToString).andThen(Value.VUnit)
    ),
    //TODO Change length and Array once OOP and multifile are implemented
    Identifier("length") -> Value.BuiltInFunction(args =>
      Value.VInt(args.head.asArray.length)
    ),
    Identifier("get") -> Value.BuiltInFunction(args =>
      args(0).asArray(args(1).asInt)  
    ),
    Identifier("Array") -> Value.BuiltInFunction(args =>
      Value.VArray(args.toArray)  
    )
  ))