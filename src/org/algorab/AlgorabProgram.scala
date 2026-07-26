package org.algorab

import purelogic.Writer
import purelogic.Abort

type AlgorabProgram[+A] = (Writer[AlgorabError], Abort[Unit]) ?=> A

object AlgorabProgram:

  def apply[A](program: AlgorabProgram[A]): (Seq[AlgorabError], Option[A]) = Writer(Abort(program).toOption)