package org.algorab

import fansi.Color
import fansi.Str
import io.github.iltotore.pureparser.ParseError
import org.algorab.ast.Symbol
import org.algorab.ast.SymbolId
import org.algorab.parsing.Token
import org.algorab.resolution.ResolutionError
import org.algorab.runtime.RuntimeError
import org.algorab.show.Printer.showPosition
import org.algorab.show.Show
import org.algorab.show.ShowContext
import org.algorab.show.str
import org.algorab.typing.TypeError
import org.algorab.util.SourcePosition
import org.algorab.show.strStripMargin
import org.algorab.show.AlgorabColor

/**
 * An error of the Algorab compiler/runtime.
 */
trait AlgorabError:

  /**
   * The textual representation of this error.
   */
  def show: Show[Str]

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
    def message: Show[Str]

    override def show: Show[Str] =
      val sourcePos = Str(ShowContext.getSource(position.file).fold("")((info, _) =>
        s"${info.name}:${position.start.line+1}:${position.start.column+1}"
      )).overlay(AlgorabColor.Error)

      str"""${Str("--").overlay(AlgorabColor.Error)} [${Str("error").overlay(AlgorabColor.Error)}] $sourcePos
           #${showPosition(position, AlgorabColor.Error)}
           #$message""".strStripMargin('#')
