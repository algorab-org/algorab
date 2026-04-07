package org.algorab.runtime

import org.algorab.compiler.InstrPosition
import org.algorab.ast.Identifier

case class ClassDef(displayName: Identifier, initStart: InstrPosition)