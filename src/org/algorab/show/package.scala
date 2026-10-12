package org.algorab.show

import fansi.Str
import scala.annotation.targetName
import org.algorab.ast.Identifier

extension (context: StringContext)
  def str(args: Any*): Str =
    args
      .zip(context.parts.map(StringContext.processEscapes).tail)
      .foldLeft(Str(context.parts.head)):
        case (before, (arg, part)) =>
          val argToStr = arg match
            case str: Str => str
            case identifier: Identifier => Str(identifier).overlay(AlgorabColor.Identifier)
            case _ => Str(arg.toString)
          
          before ++ argToStr ++ part

extension (str: Str)

  def strStripMargin: Str = Str(str.render.stripMargin)
  def strStripMargin(marginChar: Char): Str = Str(str.render.stripMargin(marginChar))