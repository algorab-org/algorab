package org.algorab

import purelogic.Abort
import purelogic.Writer

type AlgorabProgram[+A] = (Writer[AlgorabError], Abort[Unit]) ?=> A

object AlgorabProgram:

  def apply[A](program: AlgorabProgram[A]): (Seq[AlgorabError], Option[A]) = Writer(Abort(program).toOption)
