package org.algorab.resolution

import org.algorab.ast.Identifier

case class ResolvedDef(name: Identifier, initialized: Boolean)