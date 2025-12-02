package org.algorab.typer

import org.algorab.ast.tpd.Type

case class Variable(tpe: Type, mutable: Boolean, boxxed: Boolean)