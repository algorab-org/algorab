package org.algorab.ast

import io.github.iltotore.pureparser.Span

sealed trait Symbol derives CanEqual:

  def reportName: String = this match
    case Symbol.Variable(_, name, _, _, _) => s"variable $name"
    case Symbol.Function(_, name, _, _) => s"function $name"
    case Symbol.Type(_, name, _, _) => s"type $name"
    case Symbol.Invalid => "<invalid>"
  
object Symbol:

  sealed trait Valid extends Symbol:
    def name: Identifier
    def qualifiedName: Option[QualifiedName]
    def span: Span

    def withQualifiedName(qualifiedName: QualifiedName): Symbol.Valid = this match
      case Variable(id, name, _, mutable, span) => Variable(id, name, Some(qualifiedName), mutable, span)
      case Function(id, name, _, span) => Function(id, name, Some(qualifiedName), span)
      case Type(id, name, _, span) => Type(id, name, Some(qualifiedName), span)
    
  case class Variable(
      id: SymbolId,
      name: Identifier,
      qualifiedName: Option[QualifiedName],
      mutable: Boolean,
      span: Span
  ) extends Valid

  case class Function(
      id: SymbolId,
      name: Identifier,
      qualifiedName: Option[QualifiedName],
      span: Span
  ) extends Valid

  case class Type(
      id: SymbolId,
      name: Identifier,
      qualifiedName: Option[QualifiedName],
      span: Span
  ) extends Valid

  case object Invalid extends Symbol