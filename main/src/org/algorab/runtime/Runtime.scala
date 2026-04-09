/** Runtime execution effect alias and runner.
  *
  * [[Runtime]] bundles the two top-level effects required to execute an Algorab program:
  *   - `Var[RuntimeContext]` – the mutable interpreter state (stack, scopes, frames, etc.).
  *   - [[Runtime.Execution]] – the I/O capabilities needed by built-in functions
  *     (`Sync` for console access, `Abort[IOException]` for I/O errors).
  *
  * Call [[Runtime.run]] at the top of the effect stack to evaluate an `Unit < Runtime`
  * by supplying an empty initial [[RuntimeContext]].
  */
package org.algorab.runtime

import java.io.IOException
import kyo.*

/** Kyo effect alias for the full interpreter context. */
type Runtime = Var[RuntimeContext] & Runtime.Execution

object Runtime:

  /** The I/O effects required by the built-in functions (console reads/writes and I/O errors). */
  type Execution = Sync & Abort[IOException]

  /** Runs a [[Runtime]]-effectful program by providing an empty initial [[RuntimeContext]].
    *
    * Handles the `Var[RuntimeContext]` layer; the `Execution` effects are left in the
    * return type for the caller (typically [[org.algorab.KyoCommandApp]] / the end-of-the-world).
    *
    * @param body the program to run
    * @tparam S   any additional effects beyond [[Runtime]]
    * @return the result of `body` with the `Var[RuntimeContext]` handled
    */
  def run[S](body: Unit < (Runtime & S)): Unit < (Execution & S) =
    body.handle(
      Var.run(RuntimeContext.empty)
    )
