package org.algorab.runtime

import org.algorab.ast.Identifier
import org.algorab.compiler.InstrPosition

case class ClassDef(displayName: Identifier, initStart: InstrPosition)
