package org.algorab.resolution

import io.github.iltotore.pureparser.Span
import org.algorab.ast.Identifier
import purelogic.*

case class ResolutionContext(scopeName: Identifier, resolved: Map[Identifier, ResolvedDef], anonymCount: Int)

object ResolutionContext:

  val default: ResolutionContext = ResolutionContext(
    scopeName = Identifier("root"),
    resolved = Map(
      Identifier("Unit") -> ResolvedDef(Identifier("Unit"), Span(0, 0), true),
      Identifier("Boolean") -> ResolvedDef(Identifier("Boolean"), Span(0, 0), true),
      Identifier("Int") -> ResolvedDef(Identifier("Int"), Span(0, 0), true),
      Identifier("Float") -> ResolvedDef(Identifier("Float"), Span(0, 0), true),
      Identifier("Char") -> ResolvedDef(Identifier("Char"), Span(0, 0), true),
      Identifier("String") -> ResolvedDef(Identifier("String"), Span(0, 0), true),
      Identifier("println") -> ResolvedDef(Identifier("println"), Span(0, 0), true),
      Identifier("readInt") -> ResolvedDef(Identifier("readInt"), Span(0, 0), true),
      Identifier("readFloat") -> ResolvedDef(Identifier("readFloat"), Span(0, 0), true)
    ),
    anonymCount = 0
  )

  def getResolvedName(name: Identifier, span: Span): Resolution[Identifier] =
    get.resolved.get(name) match
      case Some(ResolvedDef(resolvedName, declarationSpan, initialized)) =>
        if !initialized then write(ResolutionError.ForwardDeclaration(name, declarationSpan, span))
        resolvedName
      case None =>
        write(ResolutionError.UnknownName(name, span))
        Identifier.assume("<invalid>")

  def declareDef(name: Identifier, span: Span, initialized: Boolean = true): Resolution[Identifier] =
    updateAndGet(context =>
      context.copy(
        resolved = context.resolved.updated(name, ResolvedDef(Identifier.assume(s"${context.scopeName}.$name"), span, initialized))
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
