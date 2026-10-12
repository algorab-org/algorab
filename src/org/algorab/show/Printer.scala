package org.algorab.show

import fansi.EscapeAttr
import fansi.Str
import org.algorab.AlgorabError
import org.algorab.ast.typed.Type
import org.algorab.typing.TypePattern
import org.algorab.util.SourcePosition

/**
 * A pretty-printer for code elements.
 */
object Printer:

  /**
   * Pretty-print a type.
   *
   * @param tpe the type to pretty-print
   * @return the textual representation of this type
   */
  def showType(tpe: Type): Show[String] = tpe match
    case Type.Class(symbol)            => ShowContext.getSymbolName(symbol)
    case Type.Function(inputs, output) => s"${inputs.map(showType).mkString("(", ", ", ")")} => ${showType(output)}"
    case Type.Invalid                  => "<invalid>"

  /**
   * Pretty-print a type pattern.
   *
   * @param pattern the type pattern to pretty-print
   * @return the textual representation of this type pattern
   */
  def showTypePattern(pattern: TypePattern): Show[String] = pattern match
    case TypePattern.Type(tpe)                         => showType(tpe)
    case TypePattern.Union(patterns)                   => patterns.map(showTypePattern).mkString("(", ") or (", ")")
    case TypePattern.BinaryOperator(left, right, text) => s"(${showTypePattern(left)}) $text (${showTypePattern(right)})"

  /**
   * Pretty-print a position.
   *
   * @param position the position to pretty-print
   * @return the textual representation of this position
   */
  def showPosition(position: SourcePosition, marginColor: EscapeAttr): Show[Str] = ShowContext.getSource(position.file) match
    case Some((info, source)) =>
      if position.numberOfLines == 0 then
        val span = info.lineSpans(position.start.line)
        val line = source.slice(span.start, span.end)
        val arrowLine = " " * position.start.column + "^" * (position.end.column - position.start.column)
        val positionSize = math.log10(position.start.line).toInt + 1
        println(positionSize)
        Str(s" ${position.start.line}|").overlay(marginColor) ++ line ++ Str(s"\n ${" " * positionSize}|").overlay(marginColor) ++ arrowLine
      else
        val span = info.lineSpans(position.start.line).merge(info.lineSpans(math.min(position.end.line, position.start.line + 2)))
        val lines = info.lineSpans.slice(position.start.line, math.min(position.start.line + 2, position.end.line + 1))
        val positionSize = math.log10(position.end.line).toInt + 1
        val positionMargin = Str(s"\n ${" " * positionSize}|").overlay(marginColor)
        val code = Str(lines.zipWithIndex.map((span, i) =>
          Str(s" ${position.start.line + i + 1}|").overlay(marginColor) ++ source.slice(span.start, span.end)
        ).mkString("\n"))
        if position.numberOfLines > 3 then code ++ positionMargin ++ Str("...").overlay(marginColor)
        else code ++ positionMargin

    case None => ""

  /**
   * Pretty-print the given errors.
   *
   * @param context the [[ShowContext]] to use
   * @param errors the errors to pretty-print
   * @return the textual representation of this type
   */
  def apply(context: ShowContext)(errors: Seq[AlgorabError]): Seq[Str] =
    Show(context)(errors.map(_.show))
