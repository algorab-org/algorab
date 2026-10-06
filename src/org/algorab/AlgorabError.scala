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

  def show: Show[String]

object AlgorabError:

  trait Frontend extends AlgorabError:

    def position: SourcePosition
    
    def message: Show[String]

    override def show: Show[String] =
      s"""[error] ${ShowContext.getSource(position.file).fold("")(_._1.name)}
${showPosition(position)}
$message"""