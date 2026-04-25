/**
 * Base class for Kyo-based command-line applications using the `decline` argument parser.
 *
 * [[KyoCommandApp]] combines the `decline` library's [[Command]] / [[Opts]] combinators
 * with Kyo's `KyoApp` runtime so that the main program can use the full Kyo effect stack
 * (`Async`, `Abort`, `Scope`).
 *
 * Typical usage:
 * {{{
 *   object Main extends KyoCommandApp(
 *     name    = "my-tool",
 *     header  = "Does something useful",
 *     main    = Opts.argument[Path]("input").map(run)
 *   )
 * }}}
 *
 * @see [[Main]] for the Algorab CLI entry point.
 */
package org.algorab

import com.monovore.decline.*
import kyo.*

/**
 * A `KyoApp` that parses its command-line arguments using a `decline` [[Command]].
 *
 * @param command the fully-constructed `decline` command, including name, header, and options
 */
class KyoCommandApp(command: Command[Unit < (Async & Abort[String | Throwable] & Scope)]) extends KyoApp:

  /**
   * Convenience constructor that builds the [[Command]] from its components.
   *
   * If `version` is non-empty, a `--version` flag is added (with [[Visibility.Partial]])
   * that prints the version string to stderr and exits.
   *
   * @param name      the program name shown in help output
   * @param header    a one-line description shown at the top of help output
   * @param main      the `Opts` parser for the main program
   * @param helpFlag  whether to add the standard `--help` / `-h` flag (default: `true`)
   * @param version   the version string; if empty, no `--version` flag is added (default: `""`)
   */
  def this(
      name: String,
      header: String,
      main: Opts[Unit < (Async & Abort[String | Throwable] & Scope)],
      helpFlag: Boolean = true,
      version: String = ""
  ) =
    this {
      val showVersion =
        if (version.isEmpty) Opts.never
        else
          Opts
            .flag("version", "Print the version number and exit.", visibility = Visibility.Partial)
            .map(_ => Console.printLineErr(version))

      Command(name, header, helpFlag)(showVersion.orElse(main))
    }

  /**
   * KyoApp entry point.
   *
   * Parses `args` (falling back to ambient platform args if available) using the wrapped
   * [[command]].
   *
   *   - On parse failure, the `decline` help/error text is printed to stderr.
   *   - On parse success, the resulting Kyo program is run; any `String` abort is
   *     printed to stderr via `Abort.recover`.
   */
  run:
    command.parse(PlatformApp.ambientArgs getOrElse args, sys.env) match
      case Left(help)     => Console.printLineErr(help)
      case Right(program) => Abort.recover[String](Console.printLineErr)(program)
