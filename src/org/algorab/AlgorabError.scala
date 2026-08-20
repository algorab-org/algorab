package org.algorab

import io.github.iltotore.pureparser.ParseError
import org.algorab.parsing.Token
import org.algorab.resolution.ResolutionError

type AlgorabError = ParseError[Char] | ParseError[Token] | ResolutionError
