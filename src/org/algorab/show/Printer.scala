package org.algorab.show

import org.algorab.ast.typed.Type
import org.algorab.typing.TypePattern
import org.algorab.util.SourcePosition

object Printer:

  def showType(tpe: Type): Show[String] = tpe match
    case Type.Class(symbol) => ShowContext.getSymbolName(symbol)
    case Type.Function(inputs, output) => s"${inputs.map(showType).mkString("(", ", ", ")")} => ${showType(output)}"
    case Type.Invalid => "<invalid>"

  def showTypePattern(pattern: TypePattern): Show[String] = pattern match
    case TypePattern.Type(tpe) => showType(tpe)
    case TypePattern.Union(patterns) => patterns.map(showTypePattern).mkString("(", ") or (", ")")
    case TypePattern.BinaryOperator(left, right, text) => s"(${showTypePattern(left)}) $text (${showTypePattern(right)})"

  def showPosition(position: SourcePosition): Show[String] = ShowContext.getSource(position.file) match
    case Some((info, source)) => 
      if position.numberOfLines == 0 then
        val span = info.lineSpans(position.start.line)
        val line = source.slice(span.start, span.end)
        val arrowLine = " " * (position.start.column - span.start) + "^" * (position.end.column - position.start.column)
        s"$line\n$arrowLine"

      else
        val span = info.lineSpans(position.start.line).merge(info.lineSpans(math.min(position.end.line, position.start.line + 2)))
        val lines = source.slice(span.start, span.end)
        if position.numberOfLines > 3 then s"$lines\n..."
        else lines

    case None => ""