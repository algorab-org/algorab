package org.algorab.show

import org.algorab.ast.SymbolId
import org.algorab.ast.Symbol
import org.algorab.util.FileName
import purelogic.*
import org.algorab.parsing.FileInfo

case class ShowContext(sources: Map[FileName, (FileInfo, String)], symbols: Map[SymbolId, Symbol])

object ShowContext:

  val default: ShowContext = ShowContext(
    sources = Map.empty,
    symbols = Map.empty
  )

  def getSource(name: FileName): Show[Option[(FileInfo, String)]] = read(_.sources.get(name))

  def getSymbol(id: SymbolId): Show[Option[Symbol]] = read(_.symbols.get(id))

  def getSymbolName(id: SymbolId): Show[String] = getSymbol(id).fold(s"<$id>")(_.name)