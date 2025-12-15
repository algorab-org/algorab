package org.algorab.typer

import org.algorab.ast.tpd.Type
import org.algorab.ast.Identifier

case class Variable(localName: Identifier, tpe: Type, mutable: Boolean, boxxed: Boolean)