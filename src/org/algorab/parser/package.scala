package org.algorab.parser

import io.github.iltotore.pureparser.*

def tryParser[I, A](parser: Parser[I, A]): Parser[I, Option[A]] = Parser.firstOf(Some(parser), None)
def tryParserUnit[I](parser: Parser[I, Unit]): Parser[I, Unit] = Parser.firstOf(Parser.unit(parser), ())