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

  /**
    * The result of the execution of an [[AlgorabProgram]].
    *
    * @param errors the errors produced during the program execution
    * @param showContext the generated [[ShowContext]] for pretty-printing errors
    * @param output the computation output if it didn't short-circuit
    */
  case class Result[+A](errors: Seq[AlgorabError], showContext: ShowContext, output: Option[A]):

    /**
      * The errors produced during the program execution, pretty printed.
      */
    lazy val printedErrors: Seq[String] = Printer(showContext)(errors)

  /**
    * Evaluate all the effects of an [[AlgorabProgram]] except the [[Console]] one.
    *
    * @param program the program to partially-evaluate
    * @return the result of the evaluation
    */
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

  /**
    * Run the given [[AlgorabProgram]] with using a mocked console.
    *
    * @param input the input to pass to the program
    * @param program the program to run
    * @return the program's result if it didn't abort, the produced errors, and the [[Console]] output
    */
  def withInput[A](input: String)(program: AlgorabProgram[A]): (String, Result[A]) =
    Console.withInput(input)(getResult(program))

  /**
    * Abort after the computation if it produced errors.
    *
    * @param program the program to evaluate
    */
  def abortIfErrors[A](program: AlgorabProgram[A]): AlgorabProgram[A] =
    val (errors, result) = capture(program)
    if errors.isEmpty then result
    else fail(())

  /**
    * Set the sources of the [[ShowContext]].
    *
    * @param sources the sources to register
    */
  def registerSources(sources: Map[FileName, (FileInfo, String)]): AlgorabProgram[Unit] =
    update(_.copy(sources = sources))

  /**
    * Set the symbols of the [[ShowContext]].
    *
    * @param symbols the symbols to register
    */
  def registerSymbols(symbols: Map[SymbolId, Symbol]): AlgorabProgram[Unit] =
    update(_.copy(symbols = symbols))
