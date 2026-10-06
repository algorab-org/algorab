package org.algorab

import org.algorab.util.Console
import purelogic.*
import org.algorab.show.ShowContext
import org.algorab.ast.SymbolId
import org.algorab.ast.Symbol
import org.algorab.util.FileName
import org.algorab.parsing.FileInfo
import org.algorab.show.Printer

/**
 * A program representation universal across all phases. Can produce 0 or more [[AlgorabError]] and stop.
 */
type AlgorabProgram[+A] = (Writer[AlgorabError], State[ShowContext], Abort[Unit], Console) ?=> A

object AlgorabProgram:

  case class Result[+A](errors: Seq[AlgorabError], showContext: ShowContext, output: Option[A]):

    lazy val printedErrors: Seq[String] = Printer(showContext)(errors)

  private def getResult[A](program: AlgorabProgram[A]): Console ?=> Result[A] =
    val (errors, (showContext, output)) = Writer(State(ShowContext.default)(Abort(program).toOption))
    Result(errors, showContext, output)

  /**
   * Run the given [[AlgorabProgram]].
   *
   * @param program the program to run
   * @return the program's result if it didn't abort, and the produced errors
   */
  def apply[A](program: AlgorabProgram[A]): Result[A] =
    Console.withStd(getResult(program))

  def withInput[A](input: String)(program: AlgorabProgram[A]): (String, Result[A]) =
    Console.withInput(input)(getResult(program))

  def abortIfErrors[A](program: AlgorabProgram[A]): AlgorabProgram[A] =
    val (errors, result) = capture(program)
    if errors.isEmpty then result
    else fail(())

  def registerSources(sources: Map[FileName, (FileInfo, String)]): AlgorabProgram[Unit] =
    update(_.copy(sources = sources))

  def registerSymbols(symbols: Map[SymbolId, Symbol]): AlgorabProgram[Unit] =
    update(_.copy(symbols = symbols))
