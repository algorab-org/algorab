package org.algorab.runtime

import kyo.*
import org.algorab.ast.Identifier
import org.algorab.compiler.InstrPosition
import org.algorab.compiler.Value

case class RuntimeContext(
    frames: Chunk[RuntimeFrame],
    functions: Map[Identifier, FunctionDef],
    classes: Map[Identifier, ClassDef]
):

  def modifyHeadFrame(f: RuntimeFrame => RuntimeFrame): RuntimeContext =
    this.copy(frames = f(frames.head) +: frames.tail)

  def modifyHeadFrameReturn[A](f: RuntimeFrame => (A, RuntimeFrame)): (A, RuntimeContext) =
    val (result, updated) = f(frames.head)
    (result, this.copy(frames = updated +: frames.tail))

  def nextInstruction: InstrPosition = frames.head.nextInstruction

  def jump(nextInstruction: InstrPosition): RuntimeContext =
    modifyHeadFrame(_.jump(nextInstruction))

  def push(value: Value): RuntimeContext =
    modifyHeadFrame(_.push(value))

  def pop: (Value, RuntimeContext) =
    modifyHeadFrameReturn(_.pop)

  def pushScope: RuntimeContext =
    modifyHeadFrame(_.pushScope)

  def popScope: RuntimeContext =
    modifyHeadFrame(_.popScope)

  def pushFrame(frame: RuntimeFrame): RuntimeContext =
    this.copy(frames = frame +: frames)

  def popFrame: RuntimeContext =
    this.copy(frames = frames.tail)

  def getVariable(name: Identifier): Option[Value] =
    frames.collectFirst(((frame: RuntimeFrame) => frame.getVariable(name)).unlift)

  def declareVariable(name: Identifier): RuntimeContext =
    modifyHeadFrame(_.declareVariable(name))

  def declareBox(name: Identifier): RuntimeContext =
    modifyHeadFrame(_.declareBox(name))

  def assignVariable(name: Identifier, value: Value): RuntimeContext =
    modifyHeadFrame(_.assignVariable(name, value))

  def getFunction(name: Identifier): Option[FunctionDef] = functions.get(name)

  def declareFunction(name: Identifier, function: FunctionDef): RuntimeContext =
    this.copy(functions = functions.updated(name, function))

  def getClass(name: Identifier): Option[ClassDef] = classes.get(name)

  def declareClass(name: Identifier, classDef: ClassDef): RuntimeContext =
    this.copy(classes = classes.updated(name, classDef))

object RuntimeContext:

  val empty: RuntimeContext = RuntimeContext(
    frames = Chunk(RuntimeFrame.root),
    functions = Map.empty,
    classes = Map.empty
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

  def pushFrame(frame: RuntimeFrame): Unit < Runtime =
    modify(_.pushFrame(frame))

  def popFrame: Unit < Runtime = modify(_.popFrame)

  def getVariable(name: Identifier): Value < Runtime =
    Var.use[RuntimeContext](
      _
        .getVariable(name)
        .getOrElse(throw AssertionError(s"Unknown variable: $name"))
    )

  def declareVariable(name: Identifier): Unit < Runtime =
    modify(_.declareVariable(name))

  def declareBox(name: Identifier): Unit < Runtime =
    modify(_.declareBox(name))

  def assignVariable(name: Identifier, value: Value): Unit < Runtime =
    modify(_.assignVariable(name, value))

  def getFunction(name: Identifier): FunctionDef < Runtime =
    Var.use[RuntimeContext](
      _
        .getFunction(name)
        .getOrElse(throw AssertionError(s"Unknown function: $name"))
    )

  def declareFunction(name: Identifier, function: FunctionDef): Unit < Runtime =
    modify(_.declareFunction(name, function))

  def getClass(name: Identifier): ClassDef < Runtime =
    Var.use[RuntimeContext](
      _
        .getClass(name)
        .getOrElse(throw AssertionError(s"Unknown class: $name"))
    )
  
  def declareClass(name: Identifier, classDef: ClassDef): Unit < Runtime =
    modify(_.declareClass(name, classDef))
