/**
 * The type-checking effect and its runner.
 *
 * [[Typing]] bundles three Kyo effects that together power the type-checker:
 *   - `Var[TypeContext]` – the mutable type environment (scope chain, function/class tables,
 *     variable metadata).
 *   - `Emit[TypeFailure]` – non-fatal error accumulation; the typer can record a failure and
 *     continue so that multiple errors are reported in a single pass.
 *   - `Abort[Unit]` – early termination when a fatal error makes further type-checking
 *     impossible (e.g. an unknown variable in the middle of an expression).
 *
 * == Error model ==
 *
 * The two-level model mirrors the approach used by production compilers:
 *   - `Typing.fail` emits a [[TypeFailure]] but lets the effect continue.  The typer can
 *     synthesise a placeholder type and keep going, maximising the number of errors reported.
 *   - `Typing.failAndAbort` emits a [[TypeFailure]] and then aborts immediately.  Used when
 *     recovery is not possible (e.g. the type of an expression is needed by a subsequent step).
 *   - `Typing.abortIfFail` runs a sub-computation and aborts the outer computation if the
 *     sub-computation emitted any failures, without re-emitting them.
 *
 * [[Typing.run]] interprets all three effects and returns a `Result[Chunk[TypeFailure], A]`.
 */
package org.algorab.typer

import kyo.*

/** Kyo effect alias for the type-checking phase. */
type Typing = Var[TypeContext] & Emit[TypeFailure] & Abort[Unit]

object Typing:

  /**
   * Runs a [[Typing]]-effectful computation, collecting all emitted failures.
   *
   * Interprets `Var[TypeContext]` with [[TypeContext.default]], then collects
   * `Emit[TypeFailure]` into a `Chunk`, and handles `Abort[Unit]` as a "stop here"
   * signal.  Returns:
   *   - `Result.Success(value)` – if no failures were emitted and the computation
   *     completed normally.
   *   - `Result.Failure(failures)` – if any failures were emitted or the computation
   *     was aborted.
   *
   * @param body the computation to type-check
   * @tparam A the result type of the computation
   * @tparam S any additional effects beyond [[Typing]]
   * @return `Result.Success(a)` or `Result.Failure(failures)`, effectful in `S`
   */
  def run[A, S](body: A < (Typing & S)): Result[Chunk[TypeFailure], A] < S =
    body.handle(
      Var.run(TypeContext.default),
      Abort.runPartialOrThrow,
      Emit.run(_)
    )
      .map((failures, out) =>
        out match
          case Result.Success(value) if failures.isEmpty => Result.Success(value)
          case _                                         => Result.Failure(failures)
      )

  /**
   * Emits a non-fatal [[TypeFailure]] and continues execution.
   *
   * The typer should synthesise a placeholder value (e.g. [[org.algorab.ast.tpd.Type.Any]])
   * after calling this so that type-checking can continue past the error.
   *
   * @param failure the failure to record
   */
  def fail(failure: TypeFailure): Unit < Typing = Emit.value(failure)

  /**
   * Emits a [[TypeFailure]] and immediately aborts the current type-checking computation.
   *
   * Use this when recovery is not possible and further type-checking would produce
   * misleading cascading errors.
   *
   * @param failure the failure to record
   * @return `Nothing` – this method never returns normally
   */
  def failAndAbort(failure: TypeFailure): Nothing < Typing =
    fail(failure).andThen(Abort.fail(()))

  /**
   * Runs `body` in an isolated emit context; aborts the outer computation if `body`
   * emitted any failures.
   *
   * Failures emitted inside `body` are re-emitted to the outer context before aborting,
   * so they still appear in the final failure list.
   *
   * @param body the sub-computation to guard
   * @tparam A the result type
   * @return the result of `body` if no failures were emitted; aborts otherwise
   */
  def abortIfFail[A](body: A < Typing): A < Typing =
    abortIfEmit.run(body)

  /**
   * An `Isolate` that re-emits collected failures and aborts if any were seen.
   *
   * Not intended for direct use; call [[abortIfFail]] instead.
   */
  private def abortIfEmit: Isolate[Emit[TypeFailure], Any, Emit[TypeFailure] & Abort[Unit]] =
    new Isolate[Emit[TypeFailure], Any, Emit[TypeFailure] & Abort[Unit]]:

      type State = Chunk[TypeFailure]

      type Transform[A] = (Chunk[TypeFailure], A)

      def capture[A, S](f: State => A < S)(using Frame) =
        f(Chunk.empty)

      def isolate[A, S](state: Chunk[TypeFailure], v: A < (S & Emit[TypeFailure]))(using Frame) =
        Emit.run(v)

      def restore[A, S](v: (Chunk[TypeFailure], A) < S)(using Frame): A < (Emit[TypeFailure] & Abort[Unit] & S) =
        for
          (state, result) <- v
          result <- Loop(state: Seq[TypeFailure]):
            case Seq() => Loop.done(result)
            case head +: tail =>
              Emit.valueWith(head)(Loop.continue(tail))
        yield
          if state.isEmpty then result
          else Abort.fail(())
      end restore
