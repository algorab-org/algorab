package org.algorab.show

import org.algorab.ast.typed.Type
import org.algorab.typing.TypePattern
import org.algorab.util.SourcePosition
import org.algorab.AlgorabError

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
    case Type.Class(symbol) => ShowContext.getSymbolName(symbol)
    case Type.Function(inputs, output) => s"${inputs.map(showType).mkString("(", ", ", ")")} => ${showType(output)}"
    case Type.Invalid => "<invalid>"

  /**
   * Pretty-print a type pattern.
   * 
   * @param pattern the type pattern to pretty-print
   * @return the textual representation of this type pattern
   */
  def showTypePattern(pattern: TypePattern): Show[String] = pattern match
    case TypePattern.Type(tpe) => showType(tpe)
    case TypePattern.Union(patterns) => patterns.map(showTypePattern).mkString("(", ") or (", ")")
    case TypePattern.BinaryOperator(left, right, text) => s"(${showTypePattern(left)}) $text (${showTypePattern(right)})"

  /**
   * Pretty-print a position.
   * 
   * @param position the position to pretty-print
   * @return the textual representation of this position
   */
  def showPosition(position: SourcePosition): Show[String] = ShowContext.getSource(position.file) match
    case Some((info, source)) => 
      if position.numberOfLines == 0 then
        val span = info.lineSpans(position.start.line)
        val line = source.slice(span.start, span.end)
        val arrowLine = " " * position.start.column + "^" * (position.end.column - position.start.column)
        s"$line\n$arrowLine"

      else
        val span = info.lineSpans(position.start.line).merge(info.lineSpans(math.min(position.end.line, position.start.line + 2)))
        val lines = source.slice(span.start, span.end)
        if position.numberOfLines > 3 then s"$lines\n..."
        else lines

    case None => ""

  /**
    * Pretty-print the given errors.
    *
    * @param context the [[ShowContext]] to use
    * @param errors the errors to pretty-print
    * @return the textual representation of this type
    */
  def apply(context: ShowContext)(errors: Seq[AlgorabError]): Seq[String] =
    Show(context)(errors.map(_.show))