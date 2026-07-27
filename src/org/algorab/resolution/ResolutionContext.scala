package org.algorab.resolution

import org.algorab.ast.Identifier
import purelogic.*

case class ResolutionContext(scopeName: Identifier, resolved: Map[Identifier, ResolvedDef], anonymCount: Int)

object ResolutionContext:

  val default: ResolutionContext = ResolutionContext(
    scopeName = Identifier("root"),
    resolved = Map(
      Identifier("Unit") -> ResolvedDef(Identifier("Unit"), true),
      Identifier("Boolean") -> ResolvedDef(Identifier("Boolean"), true),
      Identifier("Int") -> ResolvedDef(Identifier("Int"), true),
      Identifier("Float") -> ResolvedDef(Identifier("Float"), true),
      Identifier("Char") -> ResolvedDef(Identifier("Char"), true),
      Identifier("String") -> ResolvedDef(Identifier("String"), true),
      Identifier("println") -> ResolvedDef(Identifier("println"), true),
      Identifier("readInt") -> ResolvedDef(Identifier("readInt"), true),
      Identifier("readFloat") -> ResolvedDef(Identifier("readFloat"), true)
    ),
    anonymCount = 0
  )

  def getResolvedName(name: Identifier): Resolution[Identifier] =
    get.resolved.get(name) match
      case Some(ResolvedDef(resolvedName, initialized)) => 
        if !initialized then write(ResolutionError.ForwardDeclaration(name))
        resolvedName
      case None =>
        write(ResolutionError.UnknownName(name))
        Identifier.assume("<invalid>")

  def addName(name: Identifier, initialized: Boolean = true): Resolution[Identifier] =
    updateAndGet(context =>
      context.copy(
        resolved = context.resolved.updated(name, ResolvedDef(Identifier.assume(s"${context.scopeName}.$name"), initialized))
      )
    ).resolved(name).name

  def updatedResolvedDef(name: Identifier)(f: ResolvedDef => ResolvedDef): Resolution[Unit] =
    update(context =>
      val resolvedDef = context.resolved(name)
      context.copy(resolved = context.resolved.updated(name, f(resolvedDef)))
    )

  def inNewScope[A](name: Identifier)(body: Resolution[A]): Resolution[A] =
    localState(_.copy(scopeName = name))(body)

  def inNewAnonymScope[A](body: Resolution[A]): Resolution[A] =
    val scopeNum = get.anonymCount + 1
    update(_.copy(anonymCount = scopeNum))
    inNewScope(Identifier.assume(scopeNum.toString))(body)
