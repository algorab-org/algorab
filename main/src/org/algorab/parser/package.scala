/** Parser utilities shared across the [[Lexer]] and [[Parser]].
  *
  * This package provides two debugging/error-handling helpers that work on top of the
  * Kyo `Parse` effect:
  *
  *   - [[debug]] – wraps any parser to log its start position, end position, result, and
  *     accumulated errors.  Useful during development; should be removed before production use.
  *   - [[withErrorMessage]] – wraps any parser so that when it fails the failure is replaced
  *     (or supplemented) by a clearer, human-readable message.
  */
package org.algorab.parser

import kyo.*

/** Wraps `parser` with tracing output sent to standard output.
  *
  * On entry the current parse position is printed; on exit the new position, the result
  * (`Absent` for failure, `Present(value)` for success), and any accumulated error messages
  * are printed.
  *
  * ''This function is intended for development-time debugging only.''
  *
  * @param name   a label to identify this parser in the trace output
  * @param parser the parser to trace
  * @tparam A     the type of value produced on success
  * @tparam In    the input token type
  * @return the same parser, augmented with trace output
  */
def debug[A, In](name: String)(parser: A < Parse[In])(using Tag[In], Frame): A < Parse[In] =
  Parse.modifyState[A][In](state =>
    println(s"$name start ${state.input.position}")
    val (outState, result) = Parse.runState(state)(parser).eval
    val errorMsg =
      if outState.failures.isEmpty then ""
      else outState.failures.map(err => s"${err.position}: ${err.message}").mkString("\n- ", "\n- ", "")
    println(s"$name end ${outState.input.position}: $result$errorMsg")
    (outState, result.out)
  )

/** Wraps `parser` so that a failure emits `message` instead of whatever error(s) the inner
  * parser would have produced.
  *
  * The strategy is:
  *   1. Save a snapshot of the current [[ParseState]].
  *   1. Attempt `parser` without committing.
  *   1. On failure, rewind the error list to the snapshot and append a single
  *      [[ParseFailure]] carrying `message` at the snapshot position.
  *   1. On success, propagate the result unchanged.
  *
  * This is useful for providing friendlier error messages at known grammar boundaries
  * (e.g. "Invalid term" instead of a cascade of low-level token-mismatch errors).
  *
  * @param parser  the parser whose failure is to be replaced
  * @param message the human-readable replacement error message
  * @tparam A      the type of value produced on success
  * @tparam In     the input token type
  * @tparam S      any additional effects in `parser`
  * @return a parser that behaves identically to `parser` on success, and emits `message` on failure
  */
def withErrorMessage[A, In, S](parser: A < (Parse[In] & S), message: String)(using Tag[In], Frame): A < (Parse[In] & S) =
  for
    snapshot <- Parse.modifyState[ParseState[In]][In](state => (state, Present(state)))
    result <- Parse.attempt(parser)
  yield result match
    case Absent => Parse.modifyState(state =>
        (state.copy(failures = snapshot.failures :+ ParseFailure(message, snapshot.input.position)), Absent)
      )
    case Present(value) => value
