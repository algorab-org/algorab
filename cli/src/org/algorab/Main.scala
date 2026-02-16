package org.algorab

import cats.syntax.all.*
import com.monovore.decline.*
import io.github.iltotore.iron.decline.given
import java.io.File
import kyo.*

given Argument[Path] = Argument.from("path")(str => Path(str).validNel)

extension [A](opts: Opts[A])
  def orAbsent: Opts[Maybe[A]] = opts.map(Present.apply).withDefault(Absent)
  def mapN[B](f: A => B): Opts[B] = opts.map(f)


// ---

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