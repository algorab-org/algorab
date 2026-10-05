package org.algorab.util

import org.algorab.util.SourcePosition.Point

case class SourcePosition(file: FileName, start: Point, end: Point):

  val numberOfLines: Int = end.line - start.line

  val lowestColumn: Int = math.min(start.column, end.column)

  val highestColumn: Int = math.max(start.column, end.column)

  def union(other: SourcePosition): SourcePosition =
    if file == other.file then
      SourcePosition(
        file = file,
        start = Point.min(start, other.start),
        end = Point.max(end, other.end)
      )
    else throw AssertionError(s"Union of source positions of different files: $this and $other")

object SourcePosition:

  val BuiltIn: SourcePosition = SourcePosition.at(FileName("<built-in>"), 0, 0)

  def at(file: FileName, line: Int, column: Int): SourcePosition = SourcePosition(file, Point(line, column), Point(line, column))

  case class Point(line: Int, column: Int):

    def <(other: Point): Boolean =
      line < other.line || (line == other.line && column < other.column)

  object Point:

    def min(a: Point, b: Point): Point =
      if a < b then a
      else b

    def max(a: Point, b: Point): Point =
      if a < b then b
      else a
