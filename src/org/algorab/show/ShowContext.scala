package org.algorab.show

import org.algorab.ast.SymbolId
import org.algorab.ast.Symbol
import org.algorab.util.FileName
import purelogic.*
import org.algorab.parsing.FileInfo

case class ShowContext(symbols: Map[SymbolId, Symbol], sources: Map[FileName, (FileInfo, String)])

object ShowContext:

  def getSymbol(id: SymbolId): Show[Option[Symbol]] = read(_.symbols.get(id))

  def getSymbolName(id: SymbolId): Show[String] = getSymbol(id).fold(s"<$id>")(_.name)

  def getSource(name: FileName): Show[Option[(FileInfo, String)]] = read(_.sources.get(name))