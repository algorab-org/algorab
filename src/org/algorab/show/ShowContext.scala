package org.algorab.show

import org.algorab.ast.SymbolId
import org.algorab.ast.Symbol
import org.algorab.util.FileName
import purelogic.*
import org.algorab.parsing.FileInfo

/**
  * The context used for printing code elements.
  *
  * @param sources the source of each file
  * @param symbols the resolved symbols
  */
case class ShowContext(sources: Map[FileName, (FileInfo, String)], symbols: Map[SymbolId, Symbol])

object ShowContext:

  /**
    * The default [[ShowContext]].
    */
  val default: ShowContext = ShowContext(
    sources = Map.empty,
    symbols = Map.empty
  )

  /**
    * Get the information and source of a file.
    *
    * @param name the file's name
    * @return the information and content of the requested file
    */
  def getSource(name: FileName): Show[Option[(FileInfo, String)]] = read(_.sources.get(name))

  /**
    * Get the information of a symbol.
    *
    * @param id the symbol's id
    * @return the symbol's information if it exists
    */
  def getSymbol(id: SymbolId): Show[Option[Symbol]] = read(_.symbols.get(id))

  /**
    * Get the name of a symbol.
    *
    * @param id the symbol's id
    * @return the name of the symbol or a stub one if no such symbol exists
    */
  def getSymbolName(id: SymbolId): Show[String] = getSymbol(id).fold(s"<$id>")(_.name)