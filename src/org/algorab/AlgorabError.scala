package org.algorab

import io.github.iltotore.pureparser.ParseError
import org.algorab.parsing.Token
import org.algorab.resolution.ResolutionError
import org.algorab.runtime.RuntimeError
import org.algorab.typing.TypeError
import org.algorab.util.SourcePosition
import org.algorab.ast.SymbolId
import org.algorab.ast.Symbol
import org.algorab.show.Show
import org.algorab.show.ShowContext
import org.algorab.show.Printer.showPosition

/**
 * An error of the Algorab compiler/runtime.
 */
trait AlgorabError:

  /**
    * The textual representation of this error.
    */
  def show: Show[String]

object AlgorabError:

  /**
    * An error occuring during a frontend phase.
    */
  trait Frontend extends AlgorabError:

    /**
      * The source position of this expression.
      */
    def position: SourcePosition
    
    /**
      * The error message.
      */
    def message: Show[String]

    override def show: Show[String] =
      s"""[error] ${ShowContext.getSource(position.file).fold("")(_._1.name)}
${showPosition(position)}
$message"""