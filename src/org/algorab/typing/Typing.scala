package org.algorab.typing

import purelogic.*

type Typing[+A] = (State[TypeContext], Writer[TypeError], Abort[Unit]) ?=> A

object Typing:

  def apply[A](program: Typing[A]): (TypeContext, Either[Seq[TypeError], A]) =
    val (context, result) = State(TypeContext.default)(Writer(Abort(program)))
    result match
      case (_, Right(value)) => (context, Right(value))
      case (errors, Left(_)) => (context, Left(errors))

  def error(error: TypeError): Typing[Unit] = write(error)

  def errorAndAbort(error: TypeError): Typing[Nothing] =
    Typing.error(error)
    fail(())