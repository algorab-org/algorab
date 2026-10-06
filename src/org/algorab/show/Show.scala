package org.algorab.show

import purelogic.Reader
import org.algorab.ast.SymbolId
import org.algorab.ast.Symbol
import org.algorab.util.FileName
import org.algorab.parsing.FileInfo

type Show[+A] = Reader[ShowContext] ?=> A

object Show:

  def apply[A](context: ShowContext)(program: Show[A]): Show[A] =
    Reader(context)(program)