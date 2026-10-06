package org.algorab.parsing

import io.github.iltotore.pureparser.Span
import org.algorab.util.FileName

/**
  * File information used for parsing.
  *
  * @param name the name (usually a relative path) of the file to parse
  * @param lineSpans the absolute span of each 
  */
case class FileInfo(name: FileName, lineSpans: Seq[Span]):

  private val lineSpansAndNumber: Seq[(Span, Int)] =
    lineSpans
      .appended(Span(Int.MaxValue, Int.MaxValue))
      .sliding(2)
      .map(spans => Span(spans(0).start, spans(1).start))
      .zipWithIndex
      .toSeq

  /**
    * Get the line and column of the character sitting at the given 1-dimensional position.
    *
    * @param position the index of the character
    * @return the character's 2D coordinates, line and column
    */
  def lineAndColumn(position: Int): (Int, Int) =
    lineSpansAndNumber
      .collectFirst:
        case (Span(start, end), line) if position < end => (line, position - start)
      .get

object FileInfo:

  /**
    * Extract a [[FileInfo]] from the name and source.
    *
    * @param name the file's name
    * @param source the file's textual content
    * @return the [[FileInfo]] representing the given file
    */
  def fromSource(name: FileName, source: String): FileInfo = FileInfo(
    name = name,
    lineSpans =
      val breaks = """\r\n|\r|\n""".r.findAllMatchIn(source).toSeq

      breaks
        .scanLeft(0)((_, m) => m.end)
        .zip(breaks.map(_.start) :+ source.length)
        .map(Span.apply)
        .toIndexedSeq
  )