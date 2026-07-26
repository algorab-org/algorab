package org.algorab.resolution

import org.algorab.AlgorabProgram
import purelogic.*

type Resolution[+A] = (State[ResolutionContext], Writer[ResolutionError]) ?=> A

object Resolution:

  def apply[A](program: Resolution[A]): AlgorabProgram[(ResolutionContext, A)] =
    State(ResolutionContext.default)(program)