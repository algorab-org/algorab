package org.algorab.ast.tpd

import org.algorab.ast.AbsoluteId

enum Type derives CanEqual:
  case Class(name: AbsoluteId)
  case Invalid

object Type:
  val Boolean: Type = Class(AbsoluteId("Boolean"))
  val Int: Type = Class(AbsoluteId("Int"))
  val Float: Type = Class(AbsoluteId("Float"))
  val Char: Type = Class(AbsoluteId("Char"))
  val String: Type = Class(AbsoluteId("String"))
  val Unit: Type = Class(AbsoluteId("Unit"))