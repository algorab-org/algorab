/** Algorab command-line interface entry point.
  *
  * Parses a single file-path argument, validates it, reads the source code, and delegates
  * to [[runCode]].  Compilation failures are formatted via
  * [[CompilerFailure.toPrettyString]] and written to stderr before the process exits with
  * a non-zero status (propagated by [[KyoCommandApp]] via `Abort`).
  *
  * Usage:
  * {{{
  *   algorab <path>
  * }}}
  */
package org.algorab

import cats.syntax.all.*
import com.monovore.decline.*
import io.github.iltotore.iron.decline.given
import java.io.File
import kyo.*

/** A `decline` [[Argument]] instance for Kyo's `Path` type.
  *
  * Accepts any non-empty string as a valid path (no further validation at the parsing stage;
  * existence is checked at runtime by [[Main]]).
  */
given Argument[Path] = Argument.from("path")(str => Path(str).validNel)

extension [A](opts: Opts[A])
  /** Wraps an `Opts[A]` so that it returns `Maybe[A]`, defaulting to [[Absent]] when the
    * option is not provided.
    *
    * @return an `Opts[Maybe[A]]` that is `Present(a)` when `a` is provided, `Absent` otherwise
    */
  def orAbsent: Opts[Maybe[A]] = opts.map(Present.apply).withDefault(Absent)

  /** Alias for `opts.map(f)` that reads more naturally when chaining with [[mapN]].
    *
    * @param f the mapping function
    * @return the mapped `Opts[B]`
    */
  def mapN[B](f: A => B): Opts[B] = opts.map(f)

// ---

/** The main CLI entry point for the Algorab interpreter.
  *
  * Accepts a single positional argument `path` and runs the following steps:
  *   1. Checks that the path exists; aborts with an error message if not.
  *   1. Checks that the path is not a directory; aborts with an error message if so.
  *   1. Reads the file contents.
  *   1. Calls [[runCode]] and pattern-matches the result:
  *      - `Result.Success` – exits normally (exit code 0).
  *      - `Result.Failure` – formats all [[CompilerFailure]]s and aborts with the message.
  *      - `Result.Panic` – re-panics, propagating the underlying `Throwable`.
  */
object Main extends KyoCommandApp(
      name = "algorab",
      header = "Algorab CLI - Run Algorab code from the command line",
      main = (
        Opts.argument[Path]("path")
      ).mapN((input) =>
        for
          exists <- input.exists
          _ <-
            if exists then Kyo.unit
            else Abort.fail(s"Path ${input.path.mkString(File.separator)} does not exist")

          inputIsDir <- input.isDir
          _ <-
            if inputIsDir then Abort.fail(s"Path ${input.path.mkString(File.separator)} is a directory")
            else Kyo.unit

          code <- input.read
          result <- runCode(code)
          _ <- result match
            case Result.Success(_) => Kyo.unit
            case Result.Failure(failures) =>
              val prettyFailures = failures.map(_.toPrettyString).mkString("- ", "\n- ", "")
              Abort.fail(s"${failures.size} Compilation failures:\n$prettyFailures")
            case Result.Panic(throwable) => Abort.panic(throwable)
        yield ()
      )
    )
