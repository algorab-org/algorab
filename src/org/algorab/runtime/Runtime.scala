package org.algorab.runtime

import kyo.*
import java.io.IOException

type Runtime = Var[RuntimeContext] & Runtime.Execution

object Runtime:

  /**
   * Basically console interactions to be executed by the end of the world.
   */
  type Execution = Sync & Abort[IOException]

  def run[S](body: Unit < (Runtime & S)): Unit < (Execution & S) =
    body.handle(
      Var.run(RuntimeContext.empty)
    )