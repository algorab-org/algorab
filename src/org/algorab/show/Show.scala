package org.algorab.show

import purelogic.Reader
import org.algorab.ast.SymbolId
import org.algorab.ast.Symbol
import org.algorab.util.FileName
import org.algorab.parsing.FileInfo

type Show[+A] = Reader[ShowContext] ?=> A

object Show:

  def apply[A](symbols: Map[SymbolId, Symbol], sources: Map[FileName, (FileInfo, String)])(program: Show[A]): Show[A] =
    Reader(ShowContext(symbols, sources))(program)