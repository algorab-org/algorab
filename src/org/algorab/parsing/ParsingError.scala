package org.algorab.parsing

import io.github.iltotore.pureparser.ParseError
import io.github.iltotore.pureparser.ParseError.Pattern
import org.algorab.AlgorabError
import org.algorab.ast.SymbolId
import org.algorab.show.Show
import org.algorab.util.SourcePosition

case class ParsingError(expected: ParseError.Pattern[Char | Token], position: SourcePosition) extends AlgorabError.Frontend:

  private given CanEqual[Char | Token, Char | Token] = CanEqual.derived

  override def message: Show[String] = expected match
    case Pattern.Token(token)  => s"Expected: $token"
    case Pattern.Label(label)  => s"Expected: $label"
    case Pattern.SomethingElse => "Expected something else"
    case Pattern.EOF           => "Expected end of file"
