package org.algorab.util

import io.github.iltotore.iron.RefinedType
import io.github.iltotore.iron.constraint.any.Not
import io.github.iltotore.iron.constraint.string.Blank

type FileName = FileName.T
object FileName extends RefinedType[String, Not[Blank]]:

  given CanEqual[FileName, FileName] = CanEqual.derived
