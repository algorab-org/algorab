package org.algorab

import io.github.iltotore.pureparser.ParseError
import org.algorab.parsing.Token
import org.algorab.resolution.ResolutionError
import org.algorab.runtime.RuntimeError
import org.algorab.typing.TypeError

/**
 * An error of the Algorab compiler/runtime.
 */
type AlgorabError = ParseError[Char] | ParseError[Token] | ResolutionError | TypeError | RuntimeError
