package org.algorab.resolution

import org.algorab.ast.Identifier
import purelogic.*

case class ResolutionContext(scopeName: Identifier, resolvedNames: Map[Identifier, Identifier], anonymCount: Int)

object ResolutionContext:

  val default: ResolutionContext = ResolutionContext(
    scopeName = Identifier("root"),
    resolvedNames = Map(
      Identifier("Unit") -> Identifier("Unit"),
      Identifier("Boolean") -> Identifier("Boolean"),
      Identifier("Int") -> Identifier("Int"),
      Identifier("Float") -> Identifier("Float"),
      Identifier("Char") -> Identifier("Char"),
      Identifier("String") -> Identifier("String"),
      Identifier("println") -> Identifier("println"),
      Identifier("readInt") -> Identifier("readInt"),
      Identifier("readFloat") -> Identifier("readFloat")
    ),
    anonymCount = 0
  )

  def getResolvedName(name: Identifier): Resolution[Identifier] =
    get.resolvedNames.get(name) match
      case Some(resolved) => resolved
      case None =>
        write(ResolutionError.UnknownName(name))
        Identifier.assume("<invalid>")

  def addName(name: Identifier): Resolution[Identifier] =
    updateAndGet(context =>
      context.copy(
        resolvedNames = context.resolvedNames.updated(name, Identifier.assume(s"${context.scopeName}.$name"))
      )
    ).resolvedNames(name)

  def inNewScope[A](name: Identifier)(body: Resolution[A]): Resolution[A] =
    localState(_.copy(scopeName = name))(body)

  def inNewAnonymScope[A](body: Resolution[A]): Resolution[A] =
    val scopeNum = get.anonymCount + 1
    update(_.copy(anonymCount = scopeNum))
    inNewScope(Identifier.assume(scopeNum.toString))(body)
