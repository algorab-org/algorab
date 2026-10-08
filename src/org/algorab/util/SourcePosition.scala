package org.algorab.util

import org.algorab.util.SourcePosition.Point

/**
  * The position of a code element in the program's sources.
  *
  * @param file the file in which the element is
  * @param start the starting position of the element
  * @param end the ending position of the element
  */
case class SourcePosition(file: FileName, start: Point, end: Point):

  /**
    * The number of lines covered by this position.
    */
  val numberOfLines: Int = end.line - start.line

  /**
    * Get the union between this position and another.
    *
    * @param other the other position
    * @return the smallest [[SourcePosition]] covering both positions
    */
  def union(other: SourcePosition): SourcePosition =
    if file == other.file then
      SourcePosition(
        file = file,
        start = Point.min(start, other.start),
        end = Point.max(end, other.end)
      )
    else throw AssertionError(s"Union of source positions of different files: $this and $other")

object SourcePosition:

  /**
    * The stub position of a built-in element.
    */
  val BuiltIn: SourcePosition = SourcePosition.at(FileName("<built-in>"), 0, 0)

  /**
    * A source position covering a single [[Point]] in a file.
    *
    * @param file the file in which the position is
    * @param line the position's line
    * @param column the position's column
    * @return a position covering exactly (line, column).
    */
  def at(file: FileName, line: Int, column: Int): SourcePosition = SourcePosition(file, Point(line, column), Point(line, column+1))

  /**
    * A single point.
    *
    * @param line the point's line
    * @param column the point's column
    */
  case class Point(line: Int, column: Int):

    /**
      * Check if this point is smaller than the given one.
      *
      * @param other the point to check if greater or equal
      * @return true if this point appears before the other one in code
      */
    def <(other: Point): Boolean =
      line < other.line || (line == other.line && column < other.column)

  object Point:

    /**
      * Get the smaller point.
      *
      * @param a a point
      * @param b another point
      * @return `a` if `a < b`, `b` otherwise
      */
    def min(a: Point, b: Point): Point =
      if a < b then a
      else b

    /**
      * Get the smaller point.
      *
      * @param a a point
      * @param b another point
      * @return `b` if `a < b`, `a` otherwise
      */
    def max(a: Point, b: Point): Point =
      if a < b then b
      else a
